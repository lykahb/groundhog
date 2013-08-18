{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
module Database.Groundhog.Postgresql
    ( withPostgresqlPool
    , withPostgresqlConn
    , runDbConn
    , Postgresql
    , module Database.Groundhog
    , module Database.Groundhog.Generic.Sql.Functions
    ) where

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Migration hiding (MigrationPack(..))
import qualified Database.Groundhog.Generic.Migration as GM
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Generic.Sql.Functions
import qualified Database.Groundhog.Generic.PersistBackendHelpers as H

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.BuiltinTypes as PG
import qualified Database.PostgreSQL.Simple.Internal as PG
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.FromField as PGFF
import qualified Database.PostgreSQL.Simple.Types as PG
import Database.PostgreSQL.Simple.Ok (Ok (..))
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Control.Arrow ((***))
import Control.Exception (throw)
import Control.Monad (forM, liftM, liftM2, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ask)
import Data.ByteString.Char8 (pack, unpack, copy)
import Data.Char (isAlphaNum, isSpace, toUpper)
import Data.Function (on)
import Data.Int (Int64)
import Data.IORef
import Data.List (groupBy, intercalate, stripPrefix)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid
import Data.Pool
import Data.Time.LocalTime (localTimeToUTC, utc)

-- typical operations for connection: OPEN, BEGIN, COMMIT, ROLLBACK, CLOSE
newtype Postgresql = Postgresql PG.Connection

instance DbDescriptor Postgresql where
  type AutoKeyType Postgresql = Int64
  type QueryRaw Postgresql = Snippet Postgresql
  backendName _ = "postgresql"

instance SqlDb Postgresql where
  append a b = Expr $ operator 50 "||" a b

instance (MonadBaseControl IO m, MonadIO m) => PersistBackend (DbPersist Postgresql m) where
  {-# SPECIALIZE instance PersistBackend (DbPersist Postgresql IO) #-}
  type PhantomDb (DbPersist Postgresql m) = Postgresql
  insert v = insert' v
  insert_ v = insert_' v
  insertBy u v = H.insertBy escapeS queryRawTyped' u v
  insertByAll v = H.insertByAll escapeS queryRawTyped' v
  replace k v = H.replace escapeS queryRawTyped' executeRaw' (insertIntoConstructorTable False) k v
  select options = H.select escapeS queryRawTyped' "" renderCond' options
  selectAll = H.selectAll escapeS queryRawTyped'
  get k = H.get escapeS queryRawTyped' k
  getBy k = H.getBy escapeS queryRawTyped' k
  update upds cond = H.update escapeS executeRaw' renderCond' upds cond
  delete cond = H.delete escapeS executeRaw' renderCond' cond
  deleteByKey k = H.deleteByKey escapeS executeRaw' k
  count cond = H.count escapeS queryRawTyped' renderCond' cond
  countAll fakeV = H.countAll escapeS queryRawTyped' fakeV
  project p options = H.project escapeS queryRawTyped' "" renderCond' p options
  migrate fakeV = migrate' fakeV

  executeRaw _ query ps = executeRaw' (fromString query) ps
  queryRaw _ query ps f = queryRaw' (fromString query) ps f

  insertList l = insertList' l
  getList k = getList' k

instance (MonadBaseControl IO m, MonadIO m) => SchemaAnalyzer (DbPersist Postgresql m) where
  listTables schema = queryRaw' "SELECT table_name FROM information_schema.tables WHERE table_schema=coalesce(?,current_schema())" [toPrimitivePersistValue proxy schema] (mapAllRows $ return . fst . fromPurePersistValues proxy)
  listTableTriggers schema name = queryRaw' "SELECT trigger_name FROM information_schema.triggers WHERE event_object_schema=coalesce(?,current_schema()) AND event_object_table=?" [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
  analyzeTable = analyzeTable'
  analyzeTrigger schema name = do
    x <- queryRaw' "SELECT action_statement FROM information_schema.triggers WHERE trigger_schema=coalesce(?,current_schema()) AND trigger_name=?" [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] id
    case x of
      Nothing  -> return Nothing
      Just src -> return (fst $ fromPurePersistValues proxy src)
  analyzeFunction schema name = do
    x <- queryRaw' "SELECT p.prosrc FROM pg_catalog.pg_namespace n INNER JOIN pg_catalog.pg_proc p ON p.pronamespace = n.oid WHERE n.nspname = coalesce(?,current_schema()) AND p.proname = ?" [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] id
    case x of
      Nothing  -> return Nothing
      Just src -> return (fst $ fromPurePersistValues proxy src)

--{-# SPECIALIZE withPostgresqlPool :: String -> Int -> (Pool Postgresql -> IO a) -> IO a #-}
withPostgresqlPool :: (MonadBaseControl IO m, MonadIO m)
               => String -- ^ connection string
               -> Int -- ^ number of connections to open
               -> (Pool Postgresql -> m a)
               -> m a
withPostgresqlPool s connCount f = liftIO (createPool (open' s) close' 1 20 connCount) >>= f

{-# SPECIALIZE withPostgresqlConn :: String -> (Postgresql -> IO a) -> IO a #-}
withPostgresqlConn :: (MonadBaseControl IO m, MonadIO m)
               => String -- ^ connection string
               -> (Postgresql -> m a)
               -> m a
withPostgresqlConn s = bracket (liftIO $ open' s) (liftIO . close')

instance Savepoint Postgresql where
  withConnSavepoint name m (Postgresql c) = do
    let name' = fromString name
    liftIO $ PG.execute_ c $ "SAVEPOINT " <> name'
    x <- onException m (liftIO $ PG.execute_ c $ "ROLLBACK TO SAVEPOINT " <> name')
    liftIO $ PG.execute_ c $ "RELEASE SAVEPOINT" <> name'
    return x

instance ConnectionManager Postgresql Postgresql where
  withConn f conn@(Postgresql c) = do
    liftIO $ PG.begin c
    x <- onException (f conn) (liftIO $ PG.rollback c)
    liftIO $ PG.commit c
    return x
  withConnNoTransaction f conn = f conn

instance ConnectionManager (Pool Postgresql) Postgresql where
  withConn f pconn = withResource pconn (withConn f)
  withConnNoTransaction f pconn = withResource pconn (withConnNoTransaction f)

instance SingleConnectionManager Postgresql Postgresql

open' :: String -> IO Postgresql
open' s = do
  conn <- PG.connectPostgreSQL $ pack s
  PG.execute_ conn $ getStatement "SET client_min_messages TO WARNING"
  return $ Postgresql conn

close' :: Postgresql -> IO ()
close' (Postgresql conn) = PG.close conn

{-# SPECIALIZE insert' :: PersistEntity v => v -> DbPersist Postgresql IO (AutoKey v) #-}
insert' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> DbPersist Postgresql m (AutoKey v)
insert' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef v
  let constructorNum = fromPrimitivePersistValue proxy (head vals)

  liftM fst $ if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let RenderS query vals' = insertIntoConstructorTable True False (tableName escapeS e constr) constr (tail vals)
      case constrAutoKeyName constr of
        Nothing -> executeRaw' query (vals' []) >> pureFromPersistValue []
        Just _  -> do
          x <- queryRaw' query (vals' []) id
          case x of
            Just xs -> pureFromPersistValue xs
            Nothing -> pureFromPersistValue []
    else do
      let constr = constructors e !! constructorNum
      let query = "INSERT INTO " <> mainTableName escapeS e <> "(discr)VALUES(?)RETURNING(id)"
      rowid <- queryRaw' query (take 1 vals) getKey
      let RenderS cQuery vals' = insertIntoConstructorTable False True (tableName escapeS e constr) constr (rowid:tail vals)
      executeRaw' cQuery (vals' [])
      pureFromPersistValue [rowid]

{-# SPECIALIZE insert_' :: PersistEntity v => v -> DbPersist Postgresql IO () #-}
insert_' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> DbPersist Postgresql m ()
insert_' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef v
  let constructorNum = fromPrimitivePersistValue proxy (head vals)

  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let RenderS query vals' = insertIntoConstructorTable False False (tableName escapeS e constr) constr (tail vals)
      executeRaw' query (vals' [])
    else do
      let constr = constructors e !! constructorNum
      let query = "INSERT INTO " <> mainTableName escapeS e <> "(discr)VALUES(?)RETURNING(id)"
      rowid <- queryRaw' query (take 1 vals) getKey
      let RenderS cQuery vals' = insertIntoConstructorTable False True (tableName escapeS e constr) constr (rowid:tail vals)
      executeRaw' cQuery (vals' [])

insertIntoConstructorTable :: Bool -> Bool -> Utf8 -> ConstructorDef -> [PersistValue] -> RenderS db r
insertIntoConstructorTable withRet withId tName c vals = RenderS query vals' where
  query = "INSERT INTO " <> tName <> "(" <> fieldNames <> ")VALUES(" <> placeholders <> ")" <> returning
  (fields, returning) = case constrAutoKeyName c of
    Just idName -> (fields', returning') where
      fields' = if withId then (idName, dbType (0 :: Int64)):constrParams c else constrParams c
      returning' = if withRet then "RETURNING(" <> escapeS (fromString idName) <> ")" else mempty
    _           -> (constrParams c, mempty)
  fieldNames   = renderFields escapeS fields
  RenderS placeholders vals' = commasJoin $ map renderPersistValue vals

insertList' :: forall m a.(MonadBaseControl IO m, MonadIO m, PersistField a) => [a] -> DbPersist Postgresql m Int64
insertList' (l :: [a]) = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  k <- queryRaw' ("INSERT INTO " <> escapeS mainName <> " DEFAULT VALUES RETURNING(id)") [] getKey
  let valuesName = mainName <> delim' <> "values"
  let fields = [("ord", dbType (0 :: Int)), ("value", dbType (undefined :: a))]
  let query = "INSERT INTO " <> escapeS valuesName <> "(id," <> renderFields escapeS fields <> ")VALUES(?," <> renderFields (const $ fromChar '?') fields <> ")"
  let go :: Int -> [a] -> DbPersist Postgresql m ()
      go n (x:xs) = do
       x' <- toPersistValues x
       executeRaw' query $ (k:) . (toPrimitivePersistValue proxy n:) . x' $ []
       go (n + 1) xs
      go _ [] = return ()
  go 0 l
  return $ fromPrimitivePersistValue proxy k
  
getList' :: forall m a.(MonadBaseControl IO m, MonadIO m, PersistField a) => Int64 -> DbPersist Postgresql m [a]
getList' k = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  let valuesName = mainName <> delim' <> "values"
  let value = ("value", dbType (undefined :: a))
  let query = "SELECT " <> renderFields escapeS [value] <> " FROM " <> escapeS valuesName <> " WHERE id=? ORDER BY ord"
  queryRaw' query [toPrimitivePersistValue proxy k] $ mapAllRows (liftM fst . fromPersistValues)

--TODO: consider removal
{-# SPECIALIZE getKey :: RowPopper (DbPersist Postgresql IO) -> DbPersist Postgresql IO PersistValue #-}
getKey :: MonadIO m => RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m PersistValue
getKey pop = pop >>= \(Just [k]) -> return k

----------

executeRaw' :: MonadIO m => Utf8 -> [PersistValue] -> DbPersist Postgresql m ()
executeRaw' query vals = do
  --liftIO $ print $ fromUtf8 query ""
  Postgresql conn <- DbPersist ask
  let stmt = getStatement query
  liftIO $ do
    _ <- PG.execute conn stmt (map P vals)
    return ()

renderCond' :: Cond Postgresql r -> Maybe (RenderS Postgresql r)
renderCond' = renderCond escapeS renderEquals renderNotEquals where
  renderEquals a b = a <> " IS NOT DISTINCT FROM " <> b
  renderNotEquals a b = a <> " IS DISTINCT FROM " <> b

escapeS :: Utf8 -> Utf8
escapeS a = let q = fromChar '"' in q <> a <> q

delim' :: Utf8
delim' = fromChar delim

toEntityPersistValues' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Postgresql m [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

--- MIGRATION

migrate' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> Migration (DbPersist Postgresql m)
migrate' v = do
  x <- lift $ queryRaw' "SELECT current_schema()" [] id
  let schema = fst $ fromPurePersistValues proxy $ fromJust x
  migrateRecursively (migrateEntity $ migrationPack schema) (migrateList $ migrationPack schema) v

migrationPack :: (MonadBaseControl IO m, MonadIO m) => String -> GM.MigrationPack (DbPersist Postgresql m)
migrationPack currentSchema = GM.MigrationPack
  compareTypes
  (compareRefs currentSchema)
  compareUniqs
  compareDefaults
  migTriggerOnDelete
  migTriggerOnUpdate
  GM.defaultMigConstr
  escape
  "SERIAL PRIMARY KEY UNIQUE"
  mainTableId
  defaultPriority
  (\uniques refs -> ([], map AddUnique uniques ++ map AddReference refs))
  showColumn
  showAlterDb

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

migTriggerOnDelete :: (MonadBaseControl IO m, MonadIO m) => Maybe String -> String -> [(String, String)] -> DbPersist Postgresql m (Bool, [AlterDB])
migTriggerOnDelete schema name deletes = do
  let funcName = name
  let trigName = name
  func <- analyzeFunction schema funcName
  trig <- analyzeTrigger schema trigName
  let funcBody = "BEGIN " ++ concatMap snd deletes ++ "RETURN NEW;END;"
      addFunction = CreateOrReplaceFunction $ "CREATE OR REPLACE FUNCTION " ++ withSchema schema funcName ++ "() RETURNS trigger AS $$" ++ funcBody ++ "$$ LANGUAGE plpgsql"
      funcMig = case func of
        Nothing | null deletes -> []
        Nothing   -> [addFunction]
        Just body -> if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
          then [DropFunction schema funcName]
          else if body == funcBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropFunction schema funcName, addFunction]

      trigBody = "EXECUTE PROCEDURE " ++ withSchema schema funcName ++ "()"
      addTrigger = AddTriggerOnDelete schema trigName schema name trigBody
      (trigExisted, trigMig) = case trig of
        Nothing | null deletes -> (False, [])
        Nothing   -> (False, [addTrigger])
        Just body -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
          then [DropTrigger schema trigName schema name]
          else if body == trigBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropTrigger schema trigName schema name, addTrigger])
  return (trigExisted, funcMig ++ trigMig)
      
-- | Table name and a  list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: (MonadBaseControl IO m, MonadIO m) => Maybe String -> String -> [(String, String)] -> DbPersist Postgresql m [(Bool, [AlterDB])]
migTriggerOnUpdate schema name dels = forM dels $ \(fieldName, del) -> do
  let funcName = name ++ delim : fieldName
  let trigName = name ++ delim : fieldName
  func <- analyzeFunction schema funcName
  trig <- analyzeTrigger schema trigName
  let funcBody = "BEGIN " ++ del ++ "RETURN NEW;END;"
      addFunction = CreateOrReplaceFunction $ "CREATE OR REPLACE FUNCTION " ++ withSchema schema funcName ++ "() RETURNS trigger AS $$" ++ funcBody ++ "$$ LANGUAGE plpgsql"
      funcMig = case func of
        Nothing   -> [addFunction]
        Just body -> if body == funcBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropFunction schema funcName, addFunction]

      trigBody = "EXECUTE PROCEDURE " ++ withSchema schema funcName ++ "()"
      addTrigger = AddTriggerOnUpdate schema trigName schema name (Just fieldName) trigBody
      (trigExisted, trigMig) = case trig of
        Nothing   -> (False, [addTrigger])
        Just body -> (True, if body == trigBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropTrigger schema trigName schema name, addTrigger])
  return (trigExisted, funcMig ++ trigMig)
  
analyzeTable' :: (MonadBaseControl IO m, MonadIO m) => Maybe String -> String -> DbPersist Postgresql m (Maybe TableInfo)
analyzeTable' schema name = do
  table <- queryRaw' "SELECT * FROM information_schema.tables WHERE table_schema = coalesce(?, current_schema()) AND table_name = ?" [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] id
  case table of
    Just _ -> do
      -- omit primary keys
      let colQuery = "SELECT c.column_name, c.is_nullable, c.udt_name, c.column_default, c.character_maximum_length, c.numeric_precision, c.numeric_scale, c.datetime_precision, c.interval_type, a.attndims AS array_dims, te.typname AS array_elem\
\  FROM pg_catalog.pg_attribute a\
\  INNER JOIN pg_catalog.pg_class cl ON cl.oid = a.attrelid\
\  INNER JOIN pg_catalog.pg_namespace n ON n.oid = cl.relnamespace\
\  INNER JOIN information_schema.columns c ON c.column_name = a.attname AND c.table_name = cl.relname AND c.table_schema = n.nspname\
\  INNER JOIN pg_catalog.pg_type t ON t.oid = a.atttypid\
\  LEFT JOIN pg_catalog.pg_type te ON te.oid = t.typelem\
\  WHERE c.table_schema = coalesce(?, current_schema()) AND c.table_name=?\
\  ORDER BY c.ordinal_position"

      cols <- queryRaw' colQuery [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] (mapAllRows $ return . getColumn name . fst . fromPurePersistValues proxy)
      let constraintQuery = "SELECT u.constraint_name, u.column_name FROM information_schema.table_constraints tc INNER JOIN information_schema.constraint_column_usage u ON tc.constraint_catalog=u.constraint_catalog AND tc.constraint_schema=u.constraint_schema AND tc.constraint_name=u.constraint_name WHERE tc.constraint_type=? AND tc.table_schema=coalesce(?,current_schema()) AND u.table_name=? ORDER BY u.constraint_name, u.column_name"
      
      uniqConstraints <- queryRaw' constraintQuery [toPrimitivePersistValue proxy ("UNIQUE" :: String), toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
      uniqPrimary <- queryRaw' constraintQuery [toPrimitivePersistValue proxy ("PRIMARY KEY" :: String), toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
      uniqIndexes <- queryRaw' "SELECT ic.relname, a.attname FROM pg_catalog.pg_attribute a INNER JOIN pg_catalog.pg_class ic ON ic.oid = a.attrelid INNER JOIN pg_catalog.pg_index i ON i.indexrelid = ic.oid INNER JOIN pg_catalog.pg_class tc ON i.indrelid = tc.oid INNER JOIN pg_namespace sch ON sch.oid = tc.relnamespace WHERE sch.nspname = coalesce(?, current_schema()) AND tc.relname = ? AND a.attnum > 0 AND NOT a.attisdropped AND ic.oid NOT IN (SELECT conindid FROM pg_catalog.pg_constraint) AND NOT i.indisprimary AND i.indisunique ORDER BY ic.relname, a.attnum" [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
      let mkUniqs typ = map (\us -> UniqueDef' (fst $ head us) typ (map snd us)) . groupBy ((==) `on` fst)
      let uniqs = mkUniqs UniqueConstraint uniqConstraints ++ mkUniqs UniqueIndex uniqIndexes ++ mkUniqs UniquePrimary uniqPrimary
      references <- analyzeTableReferences schema name
      return $ Just $ TableInfo cols uniqs references
    Nothing -> return Nothing

getColumn :: String -> ((String, String, String, Maybe String), (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe String), (Int, Maybe String)) -> Column
getColumn _ ((column_name, is_nullable, udt_name, d), modifiers, arr_info) = Column column_name (is_nullable == "YES") t d where
  t = readSqlType udt_name modifiers arr_info

analyzeTableReferences :: (MonadBaseControl IO m, MonadIO m) => Maybe String -> String -> DbPersist Postgresql m [(Maybe String, Reference)]
analyzeTableReferences schema tName = do
  let sql = "SELECT c.conname, sch_parent.nspname, cl_parent.relname, c. confdeltype, c.confupdtype, a_child.attname AS child, a_parent.attname AS parent FROM\
\  (SELECT r.conrelid, r.confrelid, unnest(r.conkey) AS conkey, unnest(r.confkey) AS confkey, r.conname, r.confupdtype, r.confdeltype\
\    FROM pg_catalog.pg_constraint r WHERE r.contype = 'f'\
\  ) AS c\
\  INNER JOIN pg_attribute a_parent ON a_parent.attnum = c.confkey AND a_parent.attrelid = c.confrelid\
\  INNER JOIN pg_class cl_parent ON cl_parent.oid = c.confrelid\
\  INNER JOIN pg_namespace sch_parent ON sch_parent.oid = cl_parent.relnamespace\
\  INNER JOIN pg_attribute a_child ON a_child.attnum = c.conkey AND a_child.attrelid = c.conrelid\
\  INNER JOIN pg_class cl_child ON cl_child.oid = c.conrelid AND cl_child.relname = ?\
\  INNER JOIN pg_namespace sch_child ON sch_child.oid = cl_child.relnamespace AND sch_child.nspname = coalesce(?, current_schema())\
\  ORDER BY c.conname"
  x <- queryRaw' sql [toPrimitivePersistValue proxy tName, toPrimitivePersistValue proxy schema] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  -- (refName, ((parentTableSchema, parentTable, onDelete, onUpdate), (childColumn, parentColumn)))
  let mkReference xs = (Just refName, Reference parentSchema parentTable pairs (mkAction onDelete) (mkAction onUpdate)) where
        pairs = map (snd . snd) xs
        (refName, ((parentSchema, parentTable, onDelete, onUpdate), _)) = head xs
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
showAlterDb (AlterTable sch t _ _ _ alts) = Right $ concatMap (showAlterTable $ withSchema sch t) alts
showAlterDb (DropTrigger schTrg trigName schTbl tName) = Right [(False, triggerPriority, "DROP TRIGGER " ++ withSchema schTrg trigName ++ " ON " ++ withSchema schTbl tName)]
showAlterDb (AddTriggerOnDelete schTrg trigName schTbl tName body) = Right [(False, triggerPriority, "CREATE TRIGGER " ++ withSchema schTrg trigName ++ " AFTER DELETE ON " ++ withSchema schTbl tName ++ " FOR EACH ROW " ++ body)]
showAlterDb (AddTriggerOnUpdate schTrg trigName schTbl tName fName body) = Right [(False, triggerPriority, "CREATE TRIGGER " ++ withSchema schTrg trigName ++ " AFTER UPDATE OF " ++ fName' ++ " ON " ++ withSchema schTbl tName ++ " FOR EACH ROW " ++ body)] where
    fName' = maybe (error $ "showAlterDb: AddTriggerOnUpdate does not have fieldName for trigger " ++ trigName) escape fName
showAlterDb (CreateOrReplaceFunction s) = Right [(False, functionPriority, s)]
showAlterDb (DropFunction sch funcName) = Right [(False, functionPriority, "DROP FUNCTION " ++ withSchema sch funcName ++ "()")]

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
showAlterTable table (AddUnique (UniqueDef' uName UniqueConstraint cols)) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD"
  , maybe "" ((" CONSTRAINT " ++) . escape) uName
  , " UNIQUE("
  , intercalate "," $ map escape cols
  , ")"
  ])]
showAlterTable table (AddUnique (UniqueDef' uName UniqueIndex cols)) = [(False, defaultPriority, concat
  [ "CREATE UNIQUE INDEX "
  , maybe (error $ "showAlterTable: index for table " ++ table ++ " does not have a name") escape uName
  , " ON "
  , table
  , "("
  , intercalate "," $ map escape cols
  , ")"
  ])]
showAlterTable table (AddUnique (UniqueDef' uName UniquePrimary cols)) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD"
  , maybe "" ((" CONSTRAINT " ++) . escape) uName
  , " PRIMARY KEY("
  , intercalate "," $ map escape cols
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
showAlterTable table (AddReference (Reference schema tName columns onDelete onUpdate)) = [(False, referencePriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD FOREIGN KEY("
  , our
  , ") REFERENCES "
  , maybe "" (\x -> escape x ++ ".") schema ++ escape tName
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
    dbOther = DbOther . OtherTypeDef . const
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
  DbOther (OtherTypeDef f) -> f showSqlType
  DbAutoKey -> showSqlType DbInt64

compareUniqs :: UniqueDef' -> UniqueDef' -> Bool
compareUniqs (UniqueDef' _ UniquePrimary cols1) (UniqueDef' _ UniquePrimary cols2) = haveSameElems (==) cols1 cols2
-- only one of the uniques is primary
compareUniqs (UniqueDef' _ type1 _) (UniqueDef' _ type2 _) | UniquePrimary `elem` [type1, type2] = False
compareUniqs (UniqueDef' name1 type1 cols1) (UniqueDef' name2 type2 cols2) = fromMaybe True (liftM2 (==) name1 name2) && type1 == type2 && haveSameElems (==) cols1 cols2

compareRefs :: String -> (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
compareRefs currentSchema (_, Reference sch1 tbl1 pairs1 onDel1 onUpd1) (_, Reference sch2 tbl2 pairs2 onDel2 onUpd2) =
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

defaultPriority, referencePriority, functionPriority, triggerPriority :: Int
defaultPriority = 0
referencePriority = 1
functionPriority = 2
triggerPriority = 3

mainTableId :: String
mainTableId = "id"

--- MAIN

-- It is used to escape table names and columns, which can include only symbols allowed in Haskell datatypes and '$' delimiter. We need it mostly to support names that coincide with SQL keywords
escape :: String -> String
escape s = '\"' : s ++ "\""
  
getStatement :: Utf8 -> PG.Query
getStatement sql = PG.Query $ fromUtf8 sql

queryRawTyped' :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> [DbType] -> [PersistValue] -> (RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m a) -> DbPersist Postgresql m a
queryRawTyped' query _ vals f = queryRaw' query vals f

queryRaw' :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> [PersistValue] -> (RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m a) -> DbPersist Postgresql m a
queryRaw' query vals f = do
  Postgresql conn <- DbPersist ask
  rawquery <- liftIO $ PG.formatQuery conn (getStatement query) (map P vals)
  -- Take raw connection
  (ret, rowRef, rowCount, getters) <- liftIO $ PG.withConnection conn $ \rawconn -> do
    -- Execute query
    mret <- LibPQ.exec rawconn rawquery
    case mret of
      Nothing -> do
        merr <- LibPQ.errorMessage rawconn
        fail $ case merr of
                 Nothing -> "Postgresql.withStmt': unknown error"
                 Just e  -> "Postgresql.withStmt': " ++ unpack e
      Just ret -> do
        -- Check result status
        status <- LibPQ.resultStatus ret
        case status of
          LibPQ.TuplesOk -> return ()
          _ -> do
            msg <- LibPQ.resStatus status
            merr <- LibPQ.errorMessage rawconn
            fail $ "Postgresql.withStmt': bad result status " ++
                   show status ++ " (" ++ show msg ++ ")" ++
                   maybe "" ((". Error message: " ++) . unpack) merr

        -- Get number and type of columns
        cols <- LibPQ.nfields ret
        getters <- forM [0..cols-1] $ \col -> do
          oid <- LibPQ.ftype ret col
          case PG.oid2builtin oid of
            -- TODO: this is a temporary hack until postgresql-simple supports arrays and has more builtin types. Restore fail clause then.
            Nothing -> return $ getGetter PG.Unknown $
                       PG.Field ret col oid
             {- fail $ "Postgresql.withStmt': could not " ++
                              "recognize " ++ show oid ++ " of column " ++
                              show (let LibPQ.Col i = col in i) ++
                              " (counting from zero)" -}
            Just bt -> return $ getGetter bt $
                       PG.Field ret col oid
        -- Ready to go!
        rowRef   <- newIORef (LibPQ.Row 0)
        rowCount <- LibPQ.ntuples ret
        return (ret, rowRef, rowCount, getters)

  f $ liftIO $ do
    row <- atomicModifyIORef rowRef (\r -> (r+1, r))
    if row == rowCount
      then return Nothing
      else liftM Just $ forM (zip getters [0..]) $ \(getter, col) -> do
        mbs <-  {-# SCC "getvalue'" #-} LibPQ.getvalue' ret row col
        case mbs of
          Nothing -> return PersistNull
          Just bs -> do
            ok <- PGFF.runConversion (getter mbs) conn
            bs `seq` case ok of
              Errors (exc:_) -> throw exc
              Errors [] -> error "Got an Errors, but no exceptions"
              Ok v  -> return v

-- | Avoid orphan instances.
newtype P = P PersistValue

instance PGTF.ToField P where
  toField (P (PersistString t))         = PGTF.toField t
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

-- FIXME: check if those are correct and complete.
getGetter :: PG.BuiltinType -> Getter PersistValue
getGetter PG.Bool                  = convertPV PersistBool
getGetter PG.ByteA                 = convertPV (PersistByteString . unBinary)
getGetter PG.Char                  = convertPV PersistString
getGetter PG.Name                  = convertPV PersistString
getGetter PG.Int8                  = convertPV PersistInt64
getGetter PG.Int2                  = convertPV PersistInt64
getGetter PG.Int4                  = convertPV PersistInt64
getGetter PG.Text                  = convertPV PersistString
getGetter PG.Xml                   = convertPV PersistString
getGetter PG.Float4                = convertPV PersistDouble
getGetter PG.Float8                = convertPV PersistDouble
getGetter PG.AbsTime               = convertPV PersistUTCTime
getGetter PG.RelTime               = convertPV PersistUTCTime
--getGetter PG.Money                 = convertPV PersistString
getGetter PG.BpChar                = convertPV PersistString
getGetter PG.VarChar               = convertPV PersistString
getGetter PG.Date                  = convertPV PersistDay
getGetter PG.Time                  = convertPV PersistTimeOfDay
getGetter PG.Timestamp             = convertPV (PersistUTCTime . localTimeToUTC utc)
getGetter PG.TimestampTZ           = convertPV (PersistZonedTime . ZT)
getGetter PG.Bit                   = convertPV PersistInt64
getGetter PG.VarBit                = convertPV PersistInt64
getGetter PG.Numeric               = convertPV (PersistDouble . fromRational)
getGetter PG.Void                  = \_ _ -> return PersistNull
getGetter _ = \f dat -> fmap (PersistByteString . unBinary) $ case dat of
  Nothing -> PGFF.returnError PGFF.UnexpectedNull f ""
  Just str -> return $ PG.Binary $ copy $ str

unBinary :: PG.Binary a -> a
unBinary (PG.Binary x) = x

proxy :: Proxy Postgresql
proxy = error "Proxy Postgresql"

withSchema :: Maybe String -> String -> String
withSchema sch name = maybe "" (\x -> escape x ++ ".") sch ++ escape name
