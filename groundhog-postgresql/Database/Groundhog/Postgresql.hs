{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

module Database.Groundhog.Postgresql
    ( withPostgresqlPool
    , withPostgresqlConn
    , createPostgresqlPool
    , runDbConn
    , Postgresql(..)
    , module Database.Groundhog
    , module Database.Groundhog.Generic.Sql.Functions
    , explicitType
    , castType
    , distinctOn
    -- other
    , showSqlType
    ) where

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Migration hiding (MigrationPack(..))
import qualified Database.Groundhog.Generic.Migration as GM
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Generic.Sql.Functions
import qualified Database.Groundhog.Generic.PersistBackendHelpers as H

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Internal as PG
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.FromField as PGFF
import qualified Database.PostgreSQL.Simple.Types as PG
import Database.PostgreSQL.Simple.Ok (Ok (..))
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Control.Arrow ((***), second)
import Control.Exception (throw)
import Control.Monad (forM, liftM, liftM2, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State (mapStateT)
import Data.Acquire (mkAcquire)
import Data.ByteString.Char8 (pack, unpack, copy)
import Data.Char (isAlphaNum, isSpace, toUpper)
import Data.Function (on)
import Data.Int (Int64)
import Data.IORef
import Data.List (groupBy, intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Monoid hiding ((<>))
import Data.Pool
import Data.Time.LocalTime (localTimeToUTC, utc)

-- work around for no Semigroup instance of PG.Query prior to
-- postgresql-simple 0.5.3.0
import qualified Data.ByteString as B

-- typical operations for connection: OPEN, BEGIN, COMMIT, ROLLBACK, CLOSE
newtype Postgresql = Postgresql PG.Connection

instance DbDescriptor Postgresql where
  type AutoKeyType Postgresql = Int64
  type QueryRaw Postgresql = Snippet Postgresql
  backendName _ = "postgresql"

instance SqlDb Postgresql where
  append a b = mkExpr $ operator 50 "||" a b
  signum' x = mkExpr $ function "sign" [toExpr x]
  quotRem' x y = (mkExpr $ operator 70 "/" x y, mkExpr $ operator 70 "%" x y)
  equalsOperator a b = a <> " IS NOT DISTINCT FROM " <> b
  notEqualsOperator a b = a <> " IS DISTINCT FROM " <> b

instance FloatingSqlDb Postgresql where
  log' x = mkExpr $ function "ln" [toExpr x]
  logBase' b x = log (liftExpr x) / log (liftExpr b)

instance PersistBackendConn Postgresql where
  insert v = runDb' $ insert' v
  insert_ v = runDb' $ insert_' v
  insertBy u v = runDb' $ H.insertBy renderConfig queryRaw' True u v
  insertByAll v = runDb' $ H.insertByAll renderConfig queryRaw' True v
  replace k v = runDb' $ H.replace renderConfig queryRaw' executeRaw' (insertIntoConstructorTable False) k v
  replaceBy k v = runDb' $ H.replaceBy renderConfig executeRaw' k v
  select options = runDb' $ H.select renderConfig queryRaw' preColumns "" options
  selectStream options = runDb' $ H.selectStream renderConfig queryRaw' preColumns "" options
  selectAll = runDb' $ H.selectAll renderConfig queryRaw'
  selectAllStream = runDb' $ H.selectAllStream renderConfig queryRaw'
  get k = runDb' $ H.get renderConfig queryRaw' k
  getBy k = runDb' $ H.getBy renderConfig queryRaw' k
  update upds cond = runDb' $ H.update renderConfig executeRaw' upds cond
  delete cond = runDb' $ H.delete renderConfig executeRaw' cond
  deleteBy k = runDb' $ H.deleteBy renderConfig executeRaw' k
  deleteAll v = runDb' $ H.deleteAll renderConfig executeRaw' v
  count cond = runDb' $ H.count renderConfig queryRaw' cond
  countAll fakeV = runDb' $ H.countAll renderConfig queryRaw' fakeV
  project p options = runDb' $ H.project renderConfig queryRaw' preColumns "" p options
  projectStream p options = runDb' $ H.projectStream renderConfig queryRaw' preColumns "" p options
  migrate fakeV = mapStateT runDb' $ migrate' fakeV

  executeRaw _ query ps = runDb' $ executeRaw' (fromString query) ps
  queryRaw _ query ps = runDb' $ queryRaw' (fromString query) ps

  insertList l = runDb' $ insertList' l
  getList k = runDb' $ getList' k

instance SchemaAnalyzer Postgresql where
  schemaExists schema = runDb' $ queryRaw' "SELECT 1 FROM pg_catalog.pg_namespace WHERE nspname=?" [toPrimitivePersistValue schema] >>= firstRow >>= return . isJust
  getCurrentSchema = runDb' $ queryRaw' "SELECT current_schema()" [] >>= firstRow >>= return . (>>= fst . fromPurePersistValues)
  listTables schema = runDb' $ queryRaw' "SELECT table_name FROM information_schema.tables WHERE table_schema=coalesce(?,current_schema())" [toPrimitivePersistValue schema] >>= mapStream (return . fst . fromPurePersistValues) >>= streamToList
  listTableTriggers name = runDb' $ queryRaw' "SELECT trigger_name FROM information_schema.triggers WHERE event_object_schema=coalesce(?,current_schema()) AND event_object_table=?" (toPurePersistValues name []) >>= mapStream (return . fst . fromPurePersistValues) >>= streamToList
  analyzeTable = runDb' . analyzeTable'
  analyzeTrigger name = runDb' $ do
    x <- queryRaw' "SELECT action_statement FROM information_schema.triggers WHERE trigger_schema=coalesce(?,current_schema()) AND trigger_name=?" (toPurePersistValues name []) >>= firstRow
    return $ case x of
      Nothing  -> Nothing
      Just src -> fst $ fromPurePersistValues src
  analyzeFunction name = runDb' $ do
    let query = "SELECT arg_types.typname, arg_types.typndims, arg_types_te.typname, ret.typname, ret.typndims, ret_te.typname, p.prosrc\
\     FROM pg_catalog.pg_namespace n\
\     INNER JOIN pg_catalog.pg_proc p ON p.pronamespace = n.oid\
\     LEFT JOIN (SELECT oid, unnest(coalesce(proallargtypes, proargtypes)) as arg FROM pg_catalog.pg_proc) as args ON p.oid = args.oid\
\     LEFT JOIN pg_type arg_types ON arg_types.oid = args.arg\
\     LEFT JOIN pg_type arg_types_te ON arg_types_te.oid = arg_types.typelem\
\     INNER JOIN pg_type ret ON p.prorettype = ret.oid\
\     LEFT JOIN pg_type ret_te ON ret_te.oid = ret.typelem\
\     WHERE n.nspname = coalesce(?,current_schema()) AND p.proname = ?"
    result <- queryRaw' query (toPurePersistValues name []) >>= mapStream (return . fst . fromPurePersistValues) >>= streamToList
    let read' (typ, arr) = readSqlType typ (Nothing, Nothing, Nothing, Nothing, Nothing) arr
    return $ case result of
      []  -> Nothing
      ((_, (ret, src)):_) -> Just $ (Just $ map read' args, Just $ read' ret, src) where
        args = mapMaybe (\(typ, arr) -> fmap (\typ' -> (typ', arr)) typ) $ map fst result
  getMigrationPack = liftM (migrationPack . fromJust) getCurrentSchema

withPostgresqlPool :: (MonadBaseControl IO m, MonadIO m)
                   => String -- ^ connection string in keyword\/value format like "host=localhost port=5432 dbname=mydb". For more details and options see http://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-PARAMKEYWORDS
                   -> Int -- ^ number of connections to open
                   -> (Pool Postgresql -> m a)
                   -> m a
withPostgresqlPool s connCount f = createPostgresqlPool s connCount >>= f

withPostgresqlConn :: (MonadBaseControl IO m, MonadIO m)
                   => String -- ^ connection string
                   -> (Postgresql -> m a)
                   -> m a
withPostgresqlConn s = bracket (liftIO $ open' s) (liftIO . close')

createPostgresqlPool :: MonadIO m
                     => String -- ^ connection string
                     -> Int -- ^ number of connections to open
                     -> m (Pool Postgresql)
createPostgresqlPool s connCount = liftIO $ createPool (open' s) close' 1 20 connCount

-- Not sure of the best way to handle Semigroup/Monoid changes in ghc 8.4
-- here. It appears that the long SQL query text interferes with the use
-- of CPP here.
--
-- Manually copying over https://github.com/lpsmith/postgresql-simple/commit/44c0bb8dec3b71e8daefe104cf643c0c4fb26768#diff-75d19972de474bc8fa181e4733f3f0d6R94
-- but this is not really a good idea.
--
combine :: PG.Query -> PG.Query -> PG.Query
-- combine = (<>)
combine (PG.Query a) (PG.Query b) = PG.Query (B.append a b)


instance Savepoint Postgresql where
  withConnSavepoint name m (Postgresql c) = do
    let name' = fromString name
    liftIO $ PG.execute_ c $ "SAVEPOINT " `combine` name'
    x <- onException m (liftIO $ PG.execute_ c $ "ROLLBACK TO SAVEPOINT " `combine` name')
    liftIO $ PG.execute_ c $ "RELEASE SAVEPOINT" `combine` name'
    return x

instance ConnectionManager Postgresql where
  withConn f conn@(Postgresql c) = do
    liftIO $ PG.begin c
    x <- onException (f conn) (liftIO $ PG.rollback c)
    liftIO $ PG.commit c
    return x

instance TryConnectionManager Postgresql where
  tryWithConn f g conn@(Postgresql c) = do
    liftIO $ PG.begin c
    x <- g (f conn)
    case x of
      Left _ -> liftIO $ PG.rollback c
      Right _ -> liftIO $ PG.commit c
    return x

instance ExtractConnection Postgresql Postgresql where
  extractConn f conn = f conn

instance ExtractConnection (Pool Postgresql) Postgresql where
  extractConn f pconn = withResource pconn f

open' :: String -> IO Postgresql
open' s = do
  conn <- PG.connectPostgreSQL $ pack s
  PG.execute_ conn $ getStatement "SET client_min_messages TO WARNING"
  return $ Postgresql conn

close' :: Postgresql -> IO ()
close' (Postgresql conn) = PG.close conn

insert' :: (PersistEntity v) => v -> Action Postgresql (AutoKey v)
insert' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef proxy v
  let constructorNum = fromPrimitivePersistValue (head vals)

  liftM fst $ if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let RenderS query vals' = insertIntoConstructorTable True False (tableName escapeS e constr) constr (tail vals)
      case constrAutoKeyName constr of
        Nothing -> executeRaw' query (vals' []) >> pureFromPersistValue []
        Just _  -> do
          x <- queryRaw' query (vals' []) >>= firstRow
          case x of
            Just xs -> pureFromPersistValue xs
            Nothing -> pureFromPersistValue []
    else do
      let constr = constructors e !! constructorNum
      let query = "INSERT INTO " <> mainTableName escapeS e <> "(discr)VALUES(?)RETURNING(id)"
      rowid <- queryRaw' query (take 1 vals) >>= getKey
      let RenderS cQuery vals' = insertIntoConstructorTable False True (tableName escapeS e constr) constr (rowid:tail vals)
      executeRaw' cQuery (vals' [])
      pureFromPersistValue [rowid]

insert_' :: (PersistEntity v) => v -> Action Postgresql ()
insert_' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef proxy v
  let constructorNum = fromPrimitivePersistValue (head vals)

  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let RenderS query vals' = insertIntoConstructorTable False False (tableName escapeS e constr) constr (tail vals)
      executeRaw' query (vals' [])
    else do
      let constr = constructors e !! constructorNum
      let query = "INSERT INTO " <> mainTableName escapeS e <> "(discr)VALUES(?)RETURNING(id)"
      rowid <- queryRaw' query (take 1 vals) >>= getKey
      let RenderS cQuery vals' = insertIntoConstructorTable False True (tableName escapeS e constr) constr (rowid:tail vals)
      executeRaw' cQuery (vals' [])

insertIntoConstructorTable :: Bool -> Bool -> Utf8 -> ConstructorDef -> [PersistValue] -> RenderS db r
insertIntoConstructorTable withRet withId tName c vals = RenderS query vals' where
  query = "INSERT INTO " <> tName <> columnsValues <> returning
  (fields, returning) = case constrAutoKeyName c of
    Just idName -> (fields', returning') where
      fields' = if withId then (idName, dbType proxy (0 :: Int64)):constrParams c else constrParams c
      returning' = if withRet then " RETURNING(" <> escapeS (fromString idName) <> ")" else mempty
    _           -> (constrParams c, mempty)
  columnsValues = case foldr (flatten escapeS) [] fields of
    [] -> " DEFAULT VALUES"
    xs -> "(" <> commasJoin xs <> ") VALUES(" <> placeholders <> ")"
  RenderS placeholders vals' = commasJoin $ map renderPersistValue vals

insertList' :: forall a . PersistField a => [a] -> Action Postgresql Int64
insertList' (l :: [a]) = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  k <- queryRaw' ("INSERT INTO " <> escapeS mainName <> " DEFAULT VALUES RETURNING(id)") [] >>= getKey
  let valuesName = mainName <> delim' <> "values"
  let fields = [("ord", dbType proxy (0 :: Int)), ("value", dbType proxy (undefined :: a))]
  let query = "INSERT INTO " <> escapeS valuesName <> "(id," <> renderFields escapeS fields <> ")VALUES(?," <> renderFields (const $ fromChar '?') fields <> ")"
  let go :: Int -> [a] -> Action Postgresql ()
      go n (x:xs) = do
       x' <- toPersistValues x
       executeRaw' query $ (k:) . (toPrimitivePersistValue n:) . x' $ []
       go (n + 1) xs
      go _ [] = return ()
  go 0 l
  return $ fromPrimitivePersistValue k
  
getList' :: forall a . PersistField a => Int64 -> Action Postgresql [a]
getList' k = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  let valuesName = mainName <> delim' <> "values"
  let value = ("value", dbType proxy (undefined :: a))
  let query = "SELECT " <> renderFields escapeS [value] <> " FROM " <> escapeS valuesName <> " WHERE id=? ORDER BY ord"
  queryRaw' query [toPrimitivePersistValue k] >>= mapStream (liftM fst . fromPersistValues) >>= streamToList

--TODO: consider removal
getKey :: RowStream [PersistValue] -> Action Postgresql PersistValue
getKey stream = firstRow stream >>= \(Just [k]) -> return k

----------

executeRaw' :: Utf8 -> [PersistValue] -> Action Postgresql ()
executeRaw' query vals = do
--  $logDebugS "SQL" $ fromString $ show (fromUtf8 query) ++ " " ++ show vals
  Postgresql conn <- ask
  let stmt = getStatement query
  liftIO $ do
    _ <- PG.execute conn stmt (map P vals)
    return ()

renderConfig :: RenderConfig
renderConfig = RenderConfig {
    esc = escapeS
}

escapeS :: Utf8 -> Utf8
escapeS a = let q = fromChar '"' in q <> a <> q

delim' :: Utf8
delim' = fromChar delim

toEntityPersistValues' :: PersistEntity v => v -> Action Postgresql [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

--- MIGRATION

migrate' :: (PersistEntity v) => v -> Migration (Action Postgresql)
migrate' v = do
  migPack <- lift $ getMigrationPack
  migrateRecursively (migrateSchema migPack) (migrateEntity migPack) (migrateList migPack) v

migrationPack :: String -> GM.MigrationPack Postgresql
migrationPack currentSchema = m where
  m = GM.MigrationPack
    compareTypes
    (compareRefs currentSchema)
    compareUniqs
    compareDefaults
    migTriggerOnDelete
    migTriggerOnUpdate
    (GM.defaultMigConstr m)
    escape
    "BIGSERIAL PRIMARY KEY UNIQUE"
    mainTableId
    defaultPriority
    (\uniques refs -> ([], map AddUnique uniques ++ map AddReference refs))
    showSqlType
    showColumn
    showAlterDb
    NoAction
    NoAction

showColumn :: Column -> String
showColumn (Column n nu t def) = concat
    [ escape n
    , " "
    , showSqlType t
    , " "
    , if nu then "NULL" else "NOT NULL"
    , case def of
        Nothing -> ""
        Just s  -> " DEFAULT " ++ s
    ]

migTriggerOnDelete :: QualifiedName -> [(String, String)] -> Action Postgresql (Bool, [AlterDB])
migTriggerOnDelete tName deletes = do
  let funcName = tName
      trigName = tName
  func <- analyzeFunction funcName
  trig <- analyzeTrigger trigName
  let funcBody = "BEGIN " ++ concatMap snd deletes ++ "RETURN NEW;END;"
      addFunction = CreateOrReplaceFunction $ "CREATE OR REPLACE FUNCTION " ++ withSchema funcName ++ "() RETURNS trigger AS $$" ++ funcBody ++ "$$ LANGUAGE plpgsql"
      funcMig = case func of
        Nothing | null deletes -> []
        Nothing   -> [addFunction]
        Just (_, Just (DbOther (OtherTypeDef [Left "trigger"])), body) -> if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
          then [DropFunction funcName]
          else if body == funcBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropFunction funcName, addFunction]
        _ -> [] -- ignore same name functions which don't return a trigger.

      trigBody = "EXECUTE PROCEDURE " ++ withSchema funcName ++ "()"
      addTrigger = AddTriggerOnDelete trigName tName trigBody
      (trigExisted, trigMig) = case trig of
        Nothing | null deletes -> (False, [])
        Nothing   -> (False, [addTrigger])
        Just body -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
          then [DropTrigger trigName tName]
          else if body == trigBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropTrigger trigName tName, addTrigger])
  return (trigExisted, funcMig ++ trigMig)
      
-- | Table name and a list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: QualifiedName -> [(String, String)] -> Action Postgresql [(Bool, [AlterDB])]
migTriggerOnUpdate tName dels = forM dels $ \(fieldName, del) -> do
  let funcName = second (\name -> name ++ delim : fieldName) tName
  let trigName = second (\name -> name ++ delim : fieldName) tName
  func <- analyzeFunction funcName
  trig <- analyzeTrigger trigName
  let funcBody = "BEGIN " ++ del ++ "RETURN NEW;END;"
      addFunction = CreateOrReplaceFunction $ "CREATE OR REPLACE FUNCTION " ++ withSchema funcName ++ "() RETURNS trigger AS $$" ++ funcBody ++ "$$ LANGUAGE plpgsql"
      funcMig = case func of
        Nothing   -> [addFunction]
        Just (_, Just (DbOther (OtherTypeDef [Left "trigger"])), body) -> if body == funcBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropFunction funcName, addFunction]
        _ -> []

      trigBody = "EXECUTE PROCEDURE " ++ withSchema funcName ++ "()"
      addTrigger = AddTriggerOnUpdate trigName tName (Just fieldName) trigBody
      (trigExisted, trigMig) = case trig of
        Nothing   -> (False, [addTrigger])
        Just body -> (True, if body == trigBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropTrigger trigName tName, addTrigger])
  return (trigExisted, funcMig ++ trigMig)
  
analyzeTable' :: QualifiedName -> Action Postgresql (Maybe TableInfo)
analyzeTable' name = do
  table <- queryRaw' "SELECT * FROM information_schema.tables WHERE table_schema = coalesce(?, current_schema()) AND table_name = ?" (toPurePersistValues name []) >>= firstRow
  case table of
    Just _ -> do
      let colQuery = "SELECT c.column_name, c.is_nullable, c.udt_name, c.column_default, c.character_maximum_length, c.numeric_precision, c.numeric_scale, c.datetime_precision, c.interval_type, a.attndims AS array_dims, te.typname AS array_elem\
\  FROM pg_catalog.pg_attribute a\
\  INNER JOIN pg_catalog.pg_class cl ON cl.oid = a.attrelid\
\  INNER JOIN pg_catalog.pg_namespace n ON n.oid = cl.relnamespace\
\  INNER JOIN information_schema.columns c ON c.column_name = a.attname AND c.table_name = cl.relname AND c.table_schema = n.nspname\
\  INNER JOIN pg_catalog.pg_type t ON t.oid = a.atttypid\
\  LEFT JOIN pg_catalog.pg_type te ON te.oid = t.typelem\
\  WHERE c.table_schema = coalesce(?, current_schema()) AND c.table_name=?\
\  ORDER BY c.ordinal_position"

      cols <- queryRaw' colQuery (toPurePersistValues name []) >>= mapStream (return . getColumn . fst . fromPurePersistValues) >>= streamToList
      let constraintQuery = "SELECT u.constraint_name, u.column_name FROM information_schema.table_constraints tc INNER JOIN information_schema.constraint_column_usage u ON tc.constraint_catalog=u.constraint_catalog AND tc.constraint_schema=u.constraint_schema AND tc.constraint_name=u.constraint_name WHERE tc.constraint_type=? AND tc.table_schema=coalesce(?,current_schema()) AND u.table_name=? ORDER BY u.constraint_name, u.column_name"
      
      uniqConstraints <- queryRaw' constraintQuery (toPurePersistValues ("UNIQUE" :: String, name) []) >>= mapStream (return . fst . fromPurePersistValues) >>= streamToList
      uniqPrimary <- queryRaw' constraintQuery (toPurePersistValues ("PRIMARY KEY" :: String, name) []) >>= mapStream (return . fst . fromPurePersistValues) >>= streamToList
      -- indexes with system columns like oid are omitted
      let indexQuery = "WITH indexes as (\
\SELECT ic.oid, ic.relname,\
\    ta.attnum, ta.attname, pg_get_indexdef(i.indexrelid, ia.attnum, true) as expr\
\  FROM pg_catalog.pg_index i\
\  INNER JOIN pg_catalog.pg_class ic ON ic.oid = i.indexrelid\
\  INNER JOIN pg_catalog.pg_class tc ON i.indrelid = tc.oid\
\  INNER JOIN pg_catalog.pg_attribute ia ON ia.attrelid=ic.oid\
\  LEFT JOIN pg_catalog.pg_attribute ta ON ta.attrelid=tc.oid AND ta.attnum = i.indkey[ia.attnum-1] AND NOT ta.attisdropped\
\  INNER JOIN pg_namespace sch ON sch.oid = tc.relnamespace\
\  WHERE sch.nspname = coalesce(?, current_schema())\
\    AND tc.relname = ?\
\    AND ic.oid NOT IN (SELECT conindid FROM pg_catalog.pg_constraint)\
\    AND NOT i.indisprimary\
\    AND i.indisunique\
\  ORDER BY ic.relname, ia.attnum)\
\SELECT i.relname, i.attname, i.expr\
\  FROM indexes i\
\  INNER JOIN (SELECT oid FROM indexes\
\    GROUP BY oid\
\    HAVING every(attnum > 0 OR attnum IS NULL)) non_system ON i.oid = non_system.oid"
      uniqIndexes <- queryRaw' indexQuery (toPurePersistValues name []) >>= mapStream (return . fst . fromPurePersistValues) >>= streamToList
      let mkUniqs typ = map (\us -> UniqueDef (fst $ head us) typ (map snd us)) . groupBy ((==) `on` fst)
          isAutoincremented = case filter (\c -> colName c `elem` map snd uniqPrimary) cols of
                                [c] -> colType c `elem` [DbInt32, DbInt64] && maybe False ("nextval" `isPrefixOf`) (colDefault c)
                                _ -> False
      let uniqs = mkUniqs UniqueConstraint (map (second Left) uniqConstraints)
               ++ mkUniqs UniqueIndex (map (second $ \(col, expr) -> maybe (Right expr) Left col) uniqIndexes)
               ++ mkUniqs (UniquePrimary isAutoincremented) (map (second Left) uniqPrimary)
      references <- analyzeTableReferences name
      return $ Just $ TableInfo cols uniqs references
    Nothing -> return Nothing

getColumn :: ((String, String, String, Maybe String), (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe String), (Int, Maybe String)) -> Column
getColumn ((column_name, is_nullable, udt_name, d), modifiers, arr_info) = Column column_name (is_nullable == "YES") t d where
  t = readSqlType udt_name modifiers arr_info

analyzeTableReferences :: QualifiedName -> Action Postgresql [(Maybe String, Reference)]
analyzeTableReferences tName = do
  let sql = "SELECT c.conname, sch_parent.nspname, cl_parent.relname, c. confdeltype, c.confupdtype, a_child.attname AS child, a_parent.attname AS parent FROM\
\  (SELECT r.conrelid, r.confrelid, unnest(r.conkey) AS conkey, unnest(r.confkey) AS confkey, r.conname, r.confupdtype, r.confdeltype\
\    FROM pg_catalog.pg_constraint r WHERE r.contype = 'f'\
\  ) AS c\
\  INNER JOIN pg_attribute a_parent ON a_parent.attnum = c.confkey AND a_parent.attrelid = c.confrelid\
\  INNER JOIN pg_class cl_parent ON cl_parent.oid = c.confrelid\
\  INNER JOIN pg_namespace sch_parent ON sch_parent.oid = cl_parent.relnamespace\
\  INNER JOIN pg_attribute a_child ON a_child.attnum = c.conkey AND a_child.attrelid = c.conrelid\
\  INNER JOIN pg_class cl_child ON cl_child.oid = c.conrelid\
\  INNER JOIN pg_namespace sch_child ON sch_child.oid = cl_child.relnamespace\
\  WHERE sch_child.nspname = coalesce(?, current_schema()) AND cl_child.relname = ?\
\  ORDER BY c.conname"
  x <- queryRaw' sql (toPurePersistValues tName []) >>= mapStream (return . fst . fromPurePersistValues) >>= streamToList
  -- (refName, ((parentTableSchema, parentTable, onDelete, onUpdate), (childColumn, parentColumn)))
  let mkReference xs = (Just refName, Reference parentTable pairs (mkAction onDelete) (mkAction onUpdate)) where
        pairs = map (snd . snd) xs
        (refName, ((parentTable, onDelete, onUpdate), _)) = head xs
        mkAction c = Just $ case c of
          "a" -> NoAction
          "r" -> Restrict
          "c" -> Cascade
          "n" -> SetNull
          "d" -> SetDefault
          _ -> error $ "unknown reference action type: " ++ c
      references = map mkReference $ groupBy ((==) `on` fst) x
  return references

showAlterDb :: AlterDB -> SingleMigration
showAlterDb (AddTable s) = Right [(False, defaultPriority, s)]
showAlterDb (AlterTable t _ _ _ alts) = Right $ concatMap (showAlterTable $ withSchema t) alts
showAlterDb (DropTrigger trigName tName) = Right [(False, triggerPriority, "DROP TRIGGER " ++ withSchema trigName ++ " ON " ++ withSchema tName)]
showAlterDb (AddTriggerOnDelete trigName tName body) = Right [(False, triggerPriority, "CREATE TRIGGER " ++ withSchema trigName ++ " AFTER DELETE ON " ++ withSchema tName ++ " FOR EACH ROW " ++ body)]
showAlterDb (AddTriggerOnUpdate trigName tName fName body) = Right [(False, triggerPriority, "CREATE TRIGGER " ++ withSchema trigName ++ " AFTER UPDATE OF " ++ fName' ++ " ON " ++ withSchema tName ++ " FOR EACH ROW " ++ body)] where
    fName' = maybe (error $ "showAlterDb: AddTriggerOnUpdate does not have fieldName for trigger " ++ show trigName) escape fName
showAlterDb (CreateOrReplaceFunction s) = Right [(False, functionPriority, s)]
showAlterDb (DropFunction funcName) = Right [(False, functionPriority, "DROP FUNCTION " ++ withSchema funcName ++ "()")]
showAlterDb (CreateSchema sch ifNotExists) = Right [(False, schemaPriority, "CREATE SCHEMA " ++ ifNotExists' ++ escape sch)] where
  ifNotExists' = if ifNotExists then "IF NOT EXISTS " else ""

showAlterTable :: String -> AlterTable -> [(Bool, Int, String)]
showAlterTable table (AddColumn col) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD COLUMN "
  , showColumn col
  ])]
showAlterTable table (DropColumn name) = [(True, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " DROP COLUMN "
  , escape name
  ])]
showAlterTable table (AlterColumn col alts) = map (showAlterColumn table $ colName col) alts
showAlterTable table (AddUnique (UniqueDef uName UniqueConstraint cols)) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD"
  , maybe "" ((" CONSTRAINT " ++) . escape) uName
  , " UNIQUE("
  , intercalate "," $ map (either escape id) cols
  , ")"
  ])]
showAlterTable table (AddUnique (UniqueDef uName UniqueIndex cols)) = [(False, defaultPriority, concat
  [ "CREATE UNIQUE INDEX "
  , maybe "" escape uName
  , " ON "
  , table
  , "("
  , intercalate "," $ map (either escape id) cols
  , ")"
  ])]
showAlterTable table (AddUnique (UniqueDef uName (UniquePrimary _) cols)) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD"
  , maybe "" ((" CONSTRAINT " ++) . escape) uName
  , " PRIMARY KEY("
  , intercalate "," $ map (either escape id) cols
  , ")"
  ])]
showAlterTable table (DropConstraint uName) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " DROP CONSTRAINT "
  , escape uName
  ])]
showAlterTable _ (DropIndex uName) = [(False, defaultPriority, concat
  [ "DROP INDEX "
  , escape uName
  ])]
showAlterTable table (AddReference (Reference tName columns onDelete onUpdate)) = [(False, referencePriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD FOREIGN KEY("
  , our
  , ") REFERENCES "
  , withSchema tName
  , "("
  , foreign
  , ")"
  , maybe "" ((" ON DELETE " ++) . showReferenceAction) onDelete
  , maybe "" ((" ON UPDATE " ++) . showReferenceAction) onUpdate
  ])] where
  (our, foreign) = f *** f $ unzip columns
  f = intercalate ", " . map escape
showAlterTable table (DropReference name) = [(False, defaultPriority,
    "ALTER TABLE " ++ table ++ " DROP CONSTRAINT " ++ name)]

showAlterColumn :: String -> String -> AlterColumn -> (Bool, Int, String)
showAlterColumn table n (Type t) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ALTER COLUMN "
  , escape n
  , " TYPE "
  , showSqlType t
  ])
showAlterColumn table n IsNull = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ALTER COLUMN "
  , escape n
  , " DROP NOT NULL"
  ])
showAlterColumn table n NotNull = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ALTER COLUMN "
  , escape n
  , " SET NOT NULL"
  ])

showAlterColumn table n (Default s) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ALTER COLUMN "
  , escape n
  , " SET DEFAULT "
  , s
  ])
showAlterColumn table n NoDefault = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ALTER COLUMN "
  , escape n
  , " DROP DEFAULT"
  ])
showAlterColumn table n (UpdateValue s) = (False, defaultPriority, concat
  [ "UPDATE "
  , table
  , " SET "
  , escape n
  , "="
  , s
  , " WHERE "
  , escape n
  , " IS NULL"
  ])

-- | udt_name, character_maximum_length, numeric_precision, numeric_scale, datetime_precision, interval_type
readSqlType :: String -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe String) -> (Int, Maybe String) -> DbTypePrimitive
readSqlType typ (character_maximum_length, numeric_precision, numeric_scale, datetime_precision, _) (array_ndims, array_elem) = (case typ of
  "int4" -> DbInt32
  "int8" -> DbInt64
  "varchar" -> maybe DbString (dbOther . ("varchar"++) . wrap . show) character_maximum_length
  "numeric" -> dbOther $ "numeric" ++ maybe "" wrap attrs where
    attrs = liftM2 (\a b -> if b == 0 then show a else show a ++ ", " ++ show b) numeric_precision numeric_scale
  "date" -> DbDay
  "bool" -> DbBool
  "time" -> mkDate DbTime "time"
  "timestamp" -> mkDate DbDayTime "timestamp"
  "timestamptz" -> mkDate DbDayTimeZoned "timestamptz"
  "float4" -> DbReal
  "float8" -> DbReal
  "bytea" -> DbBlob
  _ | array_ndims > 0 -> dbOther $ arr ++ concat (replicate array_ndims "[]") where
    arr = fromMaybe (error "readSqlType: array with elem type Nothing") array_elem
  a -> dbOther a) where
    dbOther t = DbOther $ OtherTypeDef [Left t]
    wrap x = "(" ++ x ++ ")"
    mkDate t name = maybe t (dbOther . (name++) . wrap . show) datetime_precision'
    defDateTimePrec = 6
    datetime_precision' = datetime_precision >>= \p -> if p == defDateTimePrec then Nothing else Just p

showSqlType :: DbTypePrimitive -> String
showSqlType t = case t of
  DbString -> "VARCHAR"
  DbInt32 -> "INT4"
  DbInt64 -> "INT8"
  DbReal -> "DOUBLE PRECISION"
  DbBool -> "BOOLEAN"
  DbDay -> "DATE"
  DbTime -> "TIME"
  DbDayTime -> "TIMESTAMP"
  DbDayTimeZoned -> "TIMESTAMP WITH TIME ZONE"
  DbBlob -> "BYTEA"
  DbOther (OtherTypeDef ts) -> concatMap (either id showSqlType) ts

compareUniqs :: UniqueDefInfo -> UniqueDefInfo -> Bool
compareUniqs (UniqueDef _ (UniquePrimary _) cols1) (UniqueDef _ (UniquePrimary _) cols2) = haveSameElems (==) cols1 cols2
compareUniqs (UniqueDef name1 type1 cols1) (UniqueDef name2 type2 cols2) = fromMaybe True (liftM2 (==) name1 name2) && type1 == type2 && haveSameElems (==) cols1 cols2

compareRefs :: String -> (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
compareRefs currentSchema (_, Reference (sch1, tbl1) pairs1 onDel1 onUpd1) (_, Reference (sch2, tbl2) pairs2 onDel2 onUpd2) =
     fromMaybe currentSchema sch1 == fromMaybe currentSchema sch2
  && unescape tbl1 == unescape tbl2
  && haveSameElems (==) pairs1 pairs2
  && fromMaybe NoAction onDel1 == fromMaybe NoAction onDel2
  && fromMaybe NoAction onUpd1 == fromMaybe NoAction onUpd2 where
    unescape name = if head name == '"' && last name == '"' then tail $ init name else name

compareTypes :: DbTypePrimitive -> DbTypePrimitive -> Bool
compareTypes type1 type2 = f type1 == f type2 where
  f = map toUpper . showSqlType

compareDefaults :: String -> String -> Bool
compareDefaults def1 def2 = Just def2 `elem` [Just def1, stripType def1, stripType def1 >>= stripParens] where
  stripType = fmap reverse . stripPrefix "::" . dropWhile (\c -> isAlphaNum c || isSpace c) . reverse
  stripParens = stripPrefix "(" >=> fmap reverse . stripPrefix ")" . reverse

defaultPriority, schemaPriority, referencePriority, functionPriority, triggerPriority :: Int
defaultPriority = 1
schemaPriority = 0
referencePriority = 2
functionPriority = 3
triggerPriority = 4

mainTableId :: String
mainTableId = "id"

--- MAIN

-- It is used to escape table names and columns, which can include only symbols allowed in Haskell datatypes and '$' delimiter. We need it mostly to support names that coincide with SQL keywords
escape :: String -> String
escape s = '\"' : s ++ "\""
  
getStatement :: Utf8 -> PG.Query
getStatement sql = PG.Query $ fromUtf8 sql

queryRaw' :: Utf8 -> [PersistValue] -> Action Postgresql (RowStream [PersistValue])
queryRaw' query vals = do
--  $logDebugS "SQL" $ fromString $ show (fromUtf8 query) ++ " " ++ show vals
  Postgresql conn <- ask
  let open = do
        rawquery <- PG.formatQuery conn (getStatement query) (map P vals)
        -- Take raw connection
        (ret, rowRef, rowCount, getters) <- PG.withConnection conn $ \rawconn -> do
          -- Execute query
          mret <- LibPQ.exec rawconn rawquery
          case mret of
            Nothing -> do
              merr <- LibPQ.errorMessage rawconn
              fail $ case merr of
                       Nothing -> "Postgresql.queryRaw': unknown error"
                       Just e  -> "Postgresql.queryRaw': " ++ unpack e
            Just ret -> do
              -- Check result status
              status <- LibPQ.resultStatus ret
              case status of
                LibPQ.TuplesOk -> return ()
                _ -> do
                  msg <- LibPQ.resStatus status
                  merr <- LibPQ.errorMessage rawconn
                  fail $ "Postgresql.queryRaw': bad result status " ++
                         show status ++ " (" ++ show msg ++ ")" ++
                         maybe "" ((". Error message: " ++) . unpack) merr

              -- Get number and type of columns
              cols <- LibPQ.nfields ret
              getters <- forM [0..cols-1] $ \col -> do
                oid <- LibPQ.ftype ret col
                return $ getGetter oid $ PG.Field ret col oid
              -- Ready to go!
              rowRef   <- newIORef (LibPQ.Row 0)
              rowCount <- LibPQ.ntuples ret
              return (ret, rowRef, rowCount, getters)

        return $ do
          row <- atomicModifyIORef rowRef (\r -> (r+1, r))
          if row == rowCount
            then return Nothing
            else liftM Just $ forM (zip getters [0..]) $ \(getter, col) -> do
              mbs <- LibPQ.getvalue' ret row col
              case mbs of
                Nothing -> return PersistNull
                Just bs -> do
                  ok <- PGFF.runConversion (getter mbs) conn
                  bs `seq` case ok of
                    Errors (exc:_) -> throw exc
                    Errors [] -> error "Got an Errors, but no exceptions"
                    Ok v  -> return v
  return $ mkAcquire open (const $ return ())

-- | Avoid orphan instances.
newtype P = P PersistValue

instance PGTF.ToField P where
  toField (P (PersistString t))         = PGTF.toField t
  toField (P (PersistText t))           = PGTF.toField t
  toField (P (PersistByteString bs))    = PGTF.toField (PG.Binary bs)
  toField (P (PersistInt64 i))          = PGTF.toField i
  toField (P (PersistDouble d))         = PGTF.toField d
  toField (P (PersistBool b))           = PGTF.toField b
  toField (P (PersistDay d))            = PGTF.toField d
  toField (P (PersistTimeOfDay t))      = PGTF.toField t
  toField (P (PersistUTCTime t))        = PGTF.toField t
  toField (P (PersistZonedTime (ZT t))) = PGTF.toField t
  toField (P PersistNull)               = PGTF.toField PG.Null
  toField (P (PersistCustom _ _))       = error "toField: unexpected PersistCustom"

type Getter a = PGFF.FieldParser a

convertPV :: PGFF.FromField a => (a -> b) -> Getter b
convertPV f = (fmap f .) . PGFF.fromField

getGetter :: PG.Oid -> Getter PersistValue
getGetter (PG.Oid oid) = case oid of
  16   -> convertPV PersistBool
  17   -> convertPV (PersistByteString . unBinary)
  18   -> convertPV PersistText
  19   -> convertPV PersistText
  20   -> convertPV PersistInt64
  21   -> convertPV PersistInt64
  23   -> convertPV PersistInt64
  25   -> convertPV PersistText
  142  -> convertPV PersistText
  700  -> convertPV PersistDouble
  701  -> convertPV PersistDouble
  702  -> convertPV PersistUTCTime
  703  -> convertPV PersistUTCTime
  1042 -> convertPV PersistText
  1043 -> convertPV PersistText
  1082 -> convertPV PersistDay
  1083 -> convertPV PersistTimeOfDay
  1114 -> convertPV (PersistUTCTime . localTimeToUTC utc)
  1184 -> convertPV (PersistZonedTime . ZT)
  1560 -> convertPV PersistInt64
  1562 -> convertPV PersistInt64
  1700 -> convertPV (PersistDouble . fromRational)
  2278 -> \_ _ -> return PersistNull
  _    -> \f dat -> fmap PersistByteString $ case dat of
    Nothing -> PGFF.returnError PGFF.UnexpectedNull f ""
    Just str -> return $ copy $ str

unBinary :: PG.Binary a -> a
unBinary (PG.Binary x) = x

proxy :: proxy Postgresql
proxy = error "proxy Postgresql"

withSchema :: QualifiedName -> String
withSchema (sch, name) = maybe "" (\x -> escape x ++ ".") sch ++ escape name

-- | Put explicit type for expression. It is useful for values which are defaulted to a wrong type.
-- For example, a literal Int from a 64bit machine can be defaulted to a 32bit int by Postgresql. 
-- Also a value entered as an external string (geometry, arrays and other complex types have this representation) may need an explicit type. 
explicitType :: (Expression Postgresql r a, PersistField a) => a -> Expr Postgresql r a
explicitType a = castType a t where
  t = case dbType proxy a of
    DbTypePrimitive t' _ _ _ -> showSqlType t'
    _ -> error "explicitType: type is not primitive"

-- | Casts expression to a type. @castType value \"INT\"@ results in @value::INT@.
castType :: Expression Postgresql r a => a -> String -> Expr Postgresql r a
castType a t = mkExpr $ Snippet $ \conf _ -> ["(" <> renderExpr conf (toExpr a) <> ")::" <> fromString t] where

-- | Distinct only on certain fields or expressions. For example, @select $ CondEmpty `distinctOn` (lower EmailField, IpField)@.
distinctOn :: (db ~ Postgresql, HasSelectOptions a db r, HasDistinct a ~ HFalse, Projection' p db r p') => a -> p -> SelectOptions db r (HasLimit a) (HasOffset a) (HasOrder a) HTrue
distinctOn opts p = opts' {dbSpecificOptions = ("DISTINCT_ON", clause): dbSpecificOptions opts'} where
  opts' = getSelectOptions opts
  clause = Snippet $ \conf _ -> [commasJoin $ concatMap (renderExprExtended conf 0) $ projectionExprs p []]

preColumns :: HasSelectOptions opts Postgresql r => opts -> RenderS Postgresql r
preColumns opts = clause where
  clause = apply "DISTINCT_ON" (\t -> "DISTINCT ON (" <> t <> ")")
  apply k f = case lookup k opts' of
    Nothing -> mempty
    Just (Snippet snippet) -> f $ head $ snippet renderConfig 0
  opts' = dbSpecificOptions $ getSelectOptions opts
