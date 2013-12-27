{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}
module Database.Groundhog.MySQL
    ( withMySQLPool
    , withMySQLConn
    , createMySQLPool
    , runDbConn
    , MySQL(..)
    , module Database.Groundhog
    , module Database.Groundhog.Generic.Sql.Functions
    , MySQL.ConnectInfo(..)
    , MySQLBase.SSLInfo(..)
    , MySQL.defaultConnectInfo
    , MySQLBase.defaultSSLInfo
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

import qualified Database.MySQL.Simple        as MySQL
import qualified Database.MySQL.Simple.Param  as MySQL
import qualified Database.MySQL.Simple.Result as MySQL
import qualified Database.MySQL.Simple.Types  as MySQL

import qualified Database.MySQL.Base          as MySQLBase
import qualified Database.MySQL.Base.Types    as MySQLBase

import Control.Arrow ((***))
import Control.Monad (liftM, liftM2, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger, logDebugS)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ask)
import Data.ByteString.Char8 (ByteString)
import Data.Char (toUpper)
import Data.Function (on)
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (groupBy, intercalate, intersect, partition, stripPrefix)
import Data.Maybe (fromJust, fromMaybe)
import Data.Pool

newtype MySQL = MySQL MySQL.Connection

instance DbDescriptor MySQL where
  type AutoKeyType MySQL = Int64
  type QueryRaw MySQL = Snippet MySQL
  backendName _ = "mysql"

instance SqlDb MySQL where
  append a b = Expr $ function "concat" [toExpr a, toExpr b]

instance (MonadBaseControl IO m, MonadIO m, MonadLogger m) => PersistBackend (DbPersist MySQL m) where
  type PhantomDb (DbPersist MySQL m) = MySQL
  insert v = insert' v
  insert_ v = insert_' v
  insertBy u v = H.insertBy escapeS queryRawTyped' True u v
  insertByAll v = H.insertByAll escapeS queryRawTyped' True v
  replace k v = H.replace escapeS queryRawTyped' executeRaw' insertIntoConstructorTable k v
  select options = H.select escapeS queryRawTyped' noLimit renderCond' options
  selectAll = H.selectAll escapeS queryRawTyped'
  get k = H.get escapeS queryRawTyped' k
  getBy k = H.getBy escapeS queryRawTyped' k
  update upds cond = H.update escapeS executeRaw' renderCond' upds cond
  delete cond = H.delete escapeS executeRaw' renderCond' cond
  deleteByKey k = H.deleteByKey escapeS executeRaw' k
  count cond = H.count escapeS queryRawTyped' renderCond' cond
  countAll fakeV = H.countAll escapeS queryRawTyped' fakeV
  project p options = H.project escapeS queryRawTyped' noLimit renderCond' p options
  migrate fakeV = migrate' fakeV

  executeRaw _ query ps = executeRaw' (fromString query) ps
  queryRaw _ query ps f = queryRaw' (fromString query) ps f

  insertList l = insertList' l
  getList k = getList' k

instance (MonadBaseControl IO m, MonadIO m, MonadLogger m) => SchemaAnalyzer (DbPersist MySQL m) where
  listTables schema = queryRaw' "SELECT table_name FROM information_schema.tables WHERE table_schema=coalesce(?,database())" [toPrimitivePersistValue proxy schema] (mapAllRows $ return . fst . fromPurePersistValues proxy)
  listTableTriggers schema name = queryRaw' "SELECT trigger_name FROM information_schema.triggers WHERE event_object_schema=coalesce(?,database()) AND event_object_table=?" [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
  analyzeTable = analyzeTable'
  analyzeTrigger schema name = do
    x <- queryRaw' "SELECT action_statement FROM information_schema.triggers WHERE trigger_schema=coalesce(?,database()) AND trigger_name=?" [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] id
    case x of
      Nothing  -> return Nothing
      Just src -> return (fst $ fromPurePersistValues proxy src)
  analyzeFunction schema name = do
    x <- queryRaw' "SELECT routine_definition FROM information_schema.routines WHERE routine_schema=coalesce(?,database())  AND routine_name=?" [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] id
    case x of
      Nothing  -> return Nothing
      Just src -> return (fst $ fromPurePersistValues proxy src)

withMySQLPool :: (MonadBaseControl IO m, MonadIO m)
              => MySQL.ConnectInfo
              -> Int -- ^ number of connections to open
              -> (Pool MySQL -> m a)
              -> m a
withMySQLPool s connCount f = createMySQLPool s connCount >>= f

createMySQLPool :: MonadIO m
                => MySQL.ConnectInfo
                -> Int -- ^ number of connections to open
                -> m (Pool MySQL)
createMySQLPool s connCount = liftIO $ createPool (open' s) close' 1 20 connCount

withMySQLConn :: (MonadBaseControl IO m, MonadIO m)
              => MySQL.ConnectInfo
              -> (MySQL -> m a)
              -> m a
withMySQLConn s = bracket (liftIO $ open' s) (liftIO . close')

instance Savepoint MySQL where
  withConnSavepoint name m (MySQL c) = do
    let name' = fromString name
    liftIO $ MySQL.execute_ c $ "SAVEPOINT " <> name'
    x <- onException m (liftIO $ MySQL.execute_ c $ "ROLLBACK TO SAVEPOINT " <> name')
    liftIO $ MySQL.execute_ c $ "RELEASE SAVEPOINT" <> name'
    return x

instance ConnectionManager MySQL MySQL where
  withConn f conn@(MySQL c) = do
    liftIO $ MySQL.execute_ c "start transaction"
    x <- onException (f conn) (liftIO $ MySQL.rollback c)
    liftIO $ MySQL.commit c
    return x
  withConnNoTransaction f conn = f conn

instance ConnectionManager (Pool MySQL) MySQL where
  withConn f pconn = withResource pconn (withConn f)
  withConnNoTransaction f pconn = withResource pconn (withConnNoTransaction f)

instance SingleConnectionManager MySQL MySQL

open' :: MySQL.ConnectInfo -> IO MySQL
open' ci = do
  conn <- MySQL.connect ci
  MySQLBase.autocommit conn False -- disable autocommit!
  return $ MySQL conn

close' :: MySQL -> IO ()
close' (MySQL conn) = MySQL.close conn

insert' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m) => v -> DbPersist MySQL m (AutoKey v)
insert' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef v
  let constructorNum = fromPrimitivePersistValue proxy (head vals)

  liftM fst $ if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let RenderS query vals' = insertIntoConstructorTable False (tableName escapeS e constr) constr (tail vals)
      executeRaw' query (vals' [])
      case constrAutoKeyName constr of
        Nothing -> pureFromPersistValue []
        Just _  -> getLastInsertId >>= \rowid -> pureFromPersistValue [rowid]
    else do
      let constr = constructors e !! constructorNum
      let query = "INSERT INTO " <> mainTableName escapeS e <> "(discr)VALUES(?)"
      executeRaw' query $ take 1 vals
      rowid <- getLastInsertId
      let RenderS cQuery vals' = insertIntoConstructorTable True (tableName escapeS e constr) constr (rowid:tail vals)
      executeRaw' cQuery (vals' [])
      pureFromPersistValue [rowid]

insert_' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m) => v -> DbPersist MySQL m ()
insert_' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef v
  let constructorNum = fromPrimitivePersistValue proxy (head vals)

  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let RenderS query vals' = insertIntoConstructorTable False (tableName escapeS e constr) constr (tail vals)
      executeRaw' query (vals' [])
    else do
      let constr = constructors e !! constructorNum
      let query = "INSERT INTO " <> mainTableName escapeS e <> "(discr)VALUES(?)"
      executeRaw' query $ take 1 vals
      rowid <- getLastInsertId
      let RenderS cQuery vals' = insertIntoConstructorTable True (tableName escapeS e constr) constr (rowid:tail vals)
      executeRaw' cQuery (vals' [])

insertIntoConstructorTable :: Bool -> Utf8 -> ConstructorDef -> [PersistValue] -> RenderS db r
insertIntoConstructorTable withId tName c vals = RenderS query vals' where
  query = "INSERT INTO " <> tName <> "(" <> fieldNames <> ")VALUES(" <> placeholders <> ")"
  fields = case constrAutoKeyName c of
    Just idName | withId -> (idName, dbType (0 :: Int64)):constrParams c
    _                    -> constrParams c
  fieldNames   = renderFields escapeS fields
  RenderS placeholders vals' = commasJoin $ map renderPersistValue vals

insertList' :: forall m a.(MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistField a) => [a] -> DbPersist MySQL m Int64
insertList' (l :: [a]) = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  executeRaw' ("INSERT INTO " <> escapeS mainName <> "()VALUES()") []
  k <- getLastInsertId
  let valuesName = mainName <> delim' <> "values"
  let fields = [("ord", dbType (0 :: Int)), ("value", dbType (undefined :: a))]
  let query = "INSERT INTO " <> escapeS valuesName <> "(id," <> renderFields escapeS fields <> ")VALUES(?," <> renderFields (const $ fromChar '?') fields <> ")"
  let go :: Int -> [a] -> DbPersist MySQL m ()
      go n (x:xs) = do
       x' <- toPersistValues x
       executeRaw' query $ (k:) . (toPrimitivePersistValue proxy n:) . x' $ []
       go (n + 1) xs
      go _ [] = return ()
  go 0 l
  return $ fromPrimitivePersistValue proxy k
  
getList' :: forall m a.(MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistField a) => Int64 -> DbPersist MySQL m [a]
getList' k = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  let valuesName = mainName <> delim' <> "values"
  let value = ("value", dbType (undefined :: a))
  let query = "SELECT " <> renderFields escapeS [value] <> " FROM " <> escapeS valuesName <> " WHERE id=? ORDER BY ord"
  queryRaw' query [toPrimitivePersistValue proxy k] $ mapAllRows (liftM fst . fromPersistValues)

getLastInsertId :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist MySQL m PersistValue
getLastInsertId = do
  x <- queryRaw' "SELECT last_insert_id()" [] id
  return $ maybe (error "getLastInsertId: Nothing") head x

----------

executeRaw' :: (MonadIO m, MonadLogger m) => Utf8 -> [PersistValue] -> DbPersist MySQL m ()
executeRaw' query vals = do
  $logDebugS "SQL" $ fromString $ show (fromUtf8 query) ++ " " ++ show vals
  MySQL conn <- DbPersist ask
  let stmt = getStatement query
  liftIO $ do
    _ <- MySQL.execute conn stmt (map P vals)
    return ()

renderCond' :: Cond MySQL r -> Maybe (RenderS MySQL r)
renderCond' = renderCond escapeS renderEquals renderNotEquals where
  renderEquals a b = a <> "<=>" <> b
  renderNotEquals a b = "NOT(" <> a <> "<=>" <> b <> ")"

escapeS :: Utf8 -> Utf8
escapeS a = let q = fromChar '`' in q <> a <> q

delim' :: Utf8
delim' = fromChar delim

toEntityPersistValues' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v) => v -> DbPersist MySQL m [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

--- MIGRATION

migrate' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m) => v -> Migration (DbPersist MySQL m)
migrate' v = do
  x <- lift $ queryRaw' "SELECT database()" [] id
  let schema = fst $ fromPurePersistValues proxy $ fromJust x
  migrateRecursively (migrateEntity $ migrationPack schema) (migrateList $ migrationPack schema) v

migrationPack :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => String -> GM.MigrationPack (DbPersist MySQL m)
migrationPack currentSchema = GM.MigrationPack
  compareTypes
  (compareRefs currentSchema)
  compareUniqs
  compareDefaults
  migTriggerOnDelete
  migTriggerOnUpdate
  GM.defaultMigConstr
  escape
  "BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY"
  mainTableId
  defaultPriority
  (\uniques refs -> ([], map AddUnique uniques ++ map AddReference refs))
  showColumn
  (showAlterDb currentSchema)
  Restrict

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

migTriggerOnDelete :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Maybe String -> String -> [(String, String)] -> DbPersist MySQL m (Bool, [AlterDB])
migTriggerOnDelete schema name deletes = do
  let addTrigger = AddTriggerOnDelete schema name schema name (concatMap snd deletes)
  x <- analyzeTrigger schema name
  return $ case x of
    Nothing | null deletes -> (False, [])
    Nothing -> (False, [addTrigger])
    Just sql -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
      then [DropTrigger schema name schema name]
      else if sql == "BEGIN " ++ concatMap snd deletes ++ "END"
        then []
        -- this can happen when an ephemeral field was added or removed.
        else [DropTrigger schema name schema name, addTrigger])

-- | Schema name, table name and a list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Maybe String -> String -> [(String, String)] -> DbPersist MySQL m [(Bool, [AlterDB])]
migTriggerOnUpdate schema name deletes = do
  let trigName = name ++ "_ON_UPDATE"
      f (fieldName, del) = "IF NOT (NEW." ++ escape fieldName ++ " <=> OLD." ++ escape fieldName ++ ") THEN " ++ del ++ " END IF;"
      trigBody = concatMap f deletes
  let addTrigger = AddTriggerOnUpdate schema trigName schema name Nothing trigBody
  x <- analyzeTrigger schema trigName
  return $ return $ case x of
    Nothing | null deletes -> (False, [])
    Nothing -> (False, [addTrigger])
    Just sql -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
      then [DropTrigger schema trigName schema name]
      else if sql == "BEGIN " ++ trigBody ++ "END"
        then []
        -- this can happen when an ephemeral field was added or removed.
        else [DropTrigger schema trigName schema name, addTrigger])
  
analyzeTable' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Maybe String -> String -> DbPersist MySQL m (Maybe TableInfo)
analyzeTable' schema name = do
  table <- queryRaw' "SELECT * FROM information_schema.tables WHERE table_schema = coalesce(?, database()) AND table_name = ?" [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] id
  case table of
    Just _ -> do
      -- omit primary keys
      let colQuery = "SELECT c.column_name, c.is_nullable, c.data_type, c.column_type, c.column_default, c.character_maximum_length, c.numeric_precision, c.numeric_scale\
\  FROM information_schema.columns c\
\  WHERE c.table_schema = coalesce(?, database()) AND c.table_name=?\
\  ORDER BY c.ordinal_position"

      cols <- queryRaw' colQuery [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] (mapAllRows $ return . getColumn name . fst . fromPurePersistValues proxy)
      -- MySQL has no difference between unique keys and indexes
      let constraintQuery = "SELECT u.constraint_name, u.column_name FROM information_schema.table_constraints tc INNER JOIN information_schema.key_column_usage u USING (constraint_catalog, constraint_schema, constraint_name, table_schema, table_name) WHERE tc.constraint_type=? AND tc.table_schema=coalesce(?,database()) AND u.table_name=? ORDER BY u.constraint_name, u.column_name"
      uniqConstraints <- queryRaw' constraintQuery [toPrimitivePersistValue proxy ("UNIQUE" :: String), toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
      uniqPrimary <- queryRaw' constraintQuery [toPrimitivePersistValue proxy ("PRIMARY KEY" :: String), toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
      let mkUniqs typ = map (\us -> UniqueDef' (fst $ head us) typ (map snd us)) . groupBy ((==) `on` fst)
      let uniqs = mkUniqs UniqueConstraint uniqConstraints ++ mkUniqs UniquePrimary uniqPrimary
      references <- analyzeTableReferences schema name
      return $ Just $ TableInfo cols uniqs references
    Nothing -> return Nothing

getColumn :: String -> ((String, String, String, String, Maybe String), (Maybe Int, Maybe Int, Maybe Int)) -> Column
getColumn _ ((column_name, is_nullable, data_type, column_type, d), modifiers) = Column column_name (is_nullable == "YES") t d where
  t = readSqlType data_type column_type modifiers

analyzeTableReferences :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Maybe String -> String -> DbPersist MySQL m [(Maybe String, Reference)]
analyzeTableReferences schema tName = do
  let sql = "SELECT tc.constraint_name, u.referenced_table_schema, u.referenced_table_name, rc.delete_rule, rc.update_rule, u.column_name, u.referenced_column_name FROM information_schema.table_constraints tc\
\  INNER JOIN information_schema.key_column_usage u USING (constraint_catalog, constraint_schema, constraint_name, table_schema, table_name)\
\  INNER JOIN information_schema.referential_constraints rc USING (constraint_catalog, constraint_schema, constraint_name)\
\  WHERE tc.constraint_type='FOREIGN KEY' AND tc.table_schema=coalesce(?, database()) AND tc.table_name=?\
\  ORDER BY tc.constraint_name"
  x <- queryRaw' sql [toPrimitivePersistValue proxy schema, toPrimitivePersistValue proxy tName] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  -- (refName, ((parentTableSchema, parentTable, onDelete, onUpdate), (childColumn, parentColumn)))
  let mkReference xs = (Just refName, Reference parentSchema parentTable pairs (mkAction onDelete) (mkAction onUpdate)) where
        pairs = map (snd . snd) xs
        (refName, ((parentSchema, parentTable, onDelete, onUpdate), _)) = head xs
        mkAction c = Just $ fromMaybe (error $ "unknown reference action type: " ++ c) $ readReferenceAction c
      references = map mkReference $ groupBy ((==) `on` fst) x
  return references

showAlterDb :: String -> AlterDB -> SingleMigration
showAlterDb _ (AddTable s) = Right [(False, defaultPriority, s)]
showAlterDb currentSchema (AlterTable sch t _ _ _ alts) = Right $ concatMap (showAlterTable currentSchema $ withSchema sch t) alts
showAlterDb _ (DropTrigger schTrg trigName _ _) = Right [(False, triggerPriority, "DROP TRIGGER " ++ withSchema schTrg trigName)]
showAlterDb _ (AddTriggerOnDelete schTrg trigName schTbl tName body) = Right [(False, triggerPriority, "CREATE TRIGGER " ++ withSchema schTrg trigName ++ " AFTER DELETE ON " ++ withSchema schTbl tName ++ " FOR EACH ROW BEGIN " ++ body ++ "END")]
showAlterDb _ (AddTriggerOnUpdate schTrg trigName schTbl tName _ body) = Right [(False, triggerPriority, "CREATE TRIGGER " ++ withSchema schTrg trigName ++ " AFTER UPDATE ON " ++ withSchema schTbl tName ++ " FOR EACH ROW BEGIN " ++ body ++ "END")]
showAlterDb _ (CreateOrReplaceFunction s) = Right [(False, functionPriority, s)]
showAlterDb _ (DropFunction sch funcName) = Right [(False, functionPriority, "DROP FUNCTION " ++ withSchema sch funcName ++ "()")]

showAlterTable :: String -> String -> AlterTable -> [(Bool, Int, String)]
showAlterTable _ table (AddColumn col) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD COLUMN "
  , showColumn col
  ])]
showAlterTable _ table (DropColumn name) = [(True, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " DROP COLUMN "
  , escape name
  ])]
showAlterTable _ table (AlterColumn col alts) = change ++ updates' where
  change = if null other then [] else [(False, defaultPriority, concat
    [ "ALTER TABLE "
    , table
    , " CHANGE "
    , escape $ colName col
    , " "
    , showColumn col
    ])]
  updates' = concatMap f updates where
    f (UpdateValue s) = [(False, defaultPriority, concat
      [ "UPDATE "
      , table
      , " SET "
      , escape $ colName col
      , "="
      , s
      , " WHERE "
      , escape $ colName col
      , " IS NULL"
      ])]
    f _ = []
  (updates, other) = partition (\a -> case a of UpdateValue _ -> True; _ -> False) alts
showAlterTable _ table (AddUnique (UniqueDef' uName UniqueConstraint cols)) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD"
  , maybe "" ((" CONSTRAINT " ++) . escape) uName
  , " UNIQUE("
  , intercalate "," $ map escape cols
  , ")"
  ])]
showAlterTable _ table (AddUnique (UniqueDef' uName UniqueIndex cols)) = [(False, defaultPriority, concat
  [ "CREATE UNIQUE INDEX "
  , maybe (error $ "showAlterTable: index for table " ++ table ++ " does not have a name") escape uName
  , " ON "
  , table
  , "("
  , intercalate "," $ map escape cols
  , ")"
  ])]
showAlterTable _ table (AddUnique (UniqueDef' uName UniquePrimary cols)) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD"
  , maybe "" ((" CONSTRAINT " ++) . escape) uName
  , " PRIMARY KEY("
  , intercalate "," $ map escape cols
  , ")"
  ])]
showAlterTable _ table (DropConstraint uName) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " DROP KEY "
  , escape uName
  ])]
showAlterTable _ _ (DropIndex uName) = [(False, defaultPriority, concat
  [ "DROP INDEX "
  , escape uName
  ])]
showAlterTable currentSchema table (AddReference (Reference schema tName columns onDelete onUpdate)) = [(False, referencePriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD FOREIGN KEY("
  , our
  , ") REFERENCES "
  , escape (fromMaybe currentSchema schema) ++ "." ++ escape tName
  , "("
  , foreign
  , ")"
  , maybe "" ((" ON DELETE " ++) . showReferenceAction) onDelete
  , maybe "" ((" ON UPDATE " ++) . showReferenceAction) onUpdate
  ])] where
  (our, foreign) = f *** f $ unzip columns
  f = intercalate ", " . map escape
showAlterTable _ table (DropReference name) = [(False, defaultPriority,
    "ALTER TABLE " ++ table ++ " DROP CONSTRAINT " ++ name)]

readSqlType :: String -> String -> (Maybe Int, Maybe Int, Maybe Int) -> DbTypePrimitive
readSqlType typ colTyp (_, numeric_precision, numeric_scale) = (case typ of
  _ | typ `elem` ["int", "short", "mediumint"] -> DbInt32
  _ | typ `elem` ["long", "longlong", "bigint"] -> DbInt64
  "float" | numAttrs == (Just 12, Nothing) -> DbReal
  "double" | numAttrs == (Just 22, Nothing) -> DbReal
  "decimal" | numAttrs == (Just 10, Just 0) -> DbReal
  "newdecimal" | numAttrs == (Just 10, Just 0) -> DbReal
  -- varchar, varstring, string always have length, so skip to colTyp
  _ | typ `elem` ["text", "tinytext", "mediumtext", "longtext"] -> DbString
  -- skip varbinary
  _ | typ `elem` ["blob", "tinyblob", "mediumblob", "longblob"] -> DbBlob
  "time" -> DbTime
  _ | typ `elem` ["datetime", "timestamp"] -> DbDayTime
  _ | typ `elem` ["date", "newdate", "year"] -> DbDay
  _ -> DbOther $ OtherTypeDef $ const colTyp
  ) where
    numAttrs = (numeric_precision, numeric_scale)

showSqlType :: DbTypePrimitive -> String
showSqlType t = case t of
  DbString -> "TEXT CHARACTER SET utf8"
  DbInt32 -> "INT"
  DbInt64 -> "BIGINT"
  DbReal -> "DOUBLE PRECISION"
  DbBool -> "TINYINT(1)"
  DbDay -> "DATE"
  DbTime -> "TIME"
  DbDayTime -> "DATETIME"
  DbDayTimeZoned -> "VARCHAR(50) CHARACTER SET utf8"
  DbBlob -> "BLOB"
  DbOther (OtherTypeDef f) -> f showSqlType
  DbAutoKey -> showSqlType DbInt64

compareUniqs :: UniqueDef' -> UniqueDef' -> Bool
compareUniqs (UniqueDef' _ UniquePrimary cols1) (UniqueDef' _ UniquePrimary cols2) = haveSameElems (==) cols1 cols2
-- only one of the uniques is primary
compareUniqs (UniqueDef' _ type1 _) (UniqueDef' _ type2 _) | UniquePrimary `elem` [type1, type2] = False
compareUniqs (UniqueDef' name1 _ cols1) (UniqueDef' name2 _ cols2) = fromMaybe True (liftM2 (==) name1 name2) && haveSameElems (==) cols1 cols2

compareRefs :: String -> (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
compareRefs currentSchema (_, Reference sch1 tbl1 pairs1 onDel1 onUpd1) (_, Reference sch2 tbl2 pairs2 onDel2 onUpd2) =
     fromMaybe currentSchema sch1 == fromMaybe currentSchema sch2
  && unescape tbl1 == unescape tbl2
  && haveSameElems (==) pairs1 pairs2
  && fromMaybe Restrict onDel1 == fromMaybe Restrict onDel2
  && fromMaybe Restrict onUpd1 == fromMaybe Restrict onUpd2 where
    unescape name = if head name == '"' && last name == '"' then tail $ init name else name

compareTypes :: DbTypePrimitive -> DbTypePrimitive -> Bool
compareTypes type1 type2 = f type1 == f type2 where
  f = map toUpper . showSqlType . hack
  hack DbDayTimeZoned = DbOther $ OtherTypeDef $ const "VARCHAR(50)"
  hack t = t

compareDefaults :: String -> String -> Bool
compareDefaults def1 def2 = not . null $ f def1 `intersect` f def2 where
  f def = [Just def, stripQuotes def]
  stripQuotes = stripPrefix "'" >=> fmap reverse . stripPrefix "'" . reverse

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
escape s = '`' : s ++ "`"
  
getStatement :: Utf8 -> MySQL.Query
getStatement sql = MySQL.Query $ fromUtf8 sql

queryRawTyped' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Utf8 -> [DbType] -> [PersistValue] -> (RowPopper (DbPersist MySQL m) -> DbPersist MySQL m a) -> DbPersist MySQL m a
queryRawTyped' query _ vals f = queryRaw' query vals f

queryRaw' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Utf8 -> [PersistValue] -> (RowPopper (DbPersist MySQL m) -> DbPersist MySQL m a) -> DbPersist MySQL m a
queryRaw' query vals func = do
  $logDebugS "SQL" $ fromString $ show (fromUtf8 query) ++ " " ++ show vals
  MySQL conn <- DbPersist ask
  liftIO $ MySQL.formatQuery conn (getStatement query) (map P vals) >>= MySQLBase.query conn
  result <- liftIO $ MySQLBase.storeResult conn
  -- Find out the type of the columns
  fields <- liftIO $ MySQLBase.fetchFields result
  let getters = [maybe PersistNull (getGetter (MySQLBase.fieldType f) f . Just) | f <- fields]
      convert = use getters where
        use (g:gs) (col:cols) = v `seq` vs `seq` (v:vs) where
          v  = g col
          vs = use gs cols
        use _ _ = []
  let go acc = do
        row <- MySQLBase.fetchRow result
        case row of
          [] -> return (acc [])
          _  -> let converted = convert row
                in converted `seq` go (acc . (converted:))
  -- TODO: this variable is ugly. Switching to pipes or conduit might help
  rowsVar <- liftIO $ flip finally (MySQLBase.freeResult result) (go id) >>= newIORef
  func $ do
    rows <- liftIO $ readIORef rowsVar
    case rows of
      [] -> return Nothing
      (x:xs) -> do
        liftIO $ writeIORef rowsVar xs
        return $ Just x

-- | Avoid orphan instances.
newtype P = P PersistValue

instance MySQL.Param P where
  render (P (PersistString t))      = MySQL.render t
  render (P (PersistByteString bs)) = MySQL.render bs
  render (P (PersistInt64 i))       = MySQL.render i
  render (P (PersistDouble d))      = MySQL.render d
  render (P (PersistBool b))        = MySQL.render b
  render (P (PersistDay d))         = MySQL.render d
  render (P (PersistTimeOfDay t))   = MySQL.render t
  render (P (PersistUTCTime t))     = MySQL.render t
  render (P (PersistZonedTime (ZT t))) = MySQL.render $ show t
  render (P PersistNull)            = MySQL.render MySQL.Null
  render (P (PersistCustom _ _))    = error "toField: unexpected PersistCustom"

type Getter a = MySQLBase.Field -> Maybe ByteString -> a

convertPV :: MySQL.Result a => (a -> b) -> Getter b
convertPV f = (f .) . MySQL.convert

getGetter :: MySQLBase.Type -> Getter PersistValue
-- Bool
getGetter MySQLBase.Tiny       = convertPV PersistBool
-- Int64
getGetter MySQLBase.Int24      = convertPV PersistInt64
getGetter MySQLBase.Short      = convertPV PersistInt64
getGetter MySQLBase.Long       = convertPV PersistInt64
getGetter MySQLBase.LongLong   = convertPV PersistInt64
-- Double
getGetter MySQLBase.Float      = convertPV PersistDouble
getGetter MySQLBase.Double     = convertPV PersistDouble
getGetter MySQLBase.Decimal    = convertPV PersistDouble
getGetter MySQLBase.NewDecimal = convertPV PersistDouble
-- ByteString and Text
getGetter MySQLBase.VarChar    = convertPV PersistByteString
getGetter MySQLBase.VarString  = convertPV PersistByteString
getGetter MySQLBase.String     = convertPV PersistByteString
getGetter MySQLBase.Blob       = convertPV PersistByteString
getGetter MySQLBase.TinyBlob   = convertPV PersistByteString
getGetter MySQLBase.MediumBlob = convertPV PersistByteString
getGetter MySQLBase.LongBlob   = convertPV PersistByteString
-- Time-related
getGetter MySQLBase.Time       = convertPV PersistTimeOfDay
getGetter MySQLBase.DateTime   = convertPV PersistUTCTime
getGetter MySQLBase.Timestamp  = convertPV PersistUTCTime
getGetter MySQLBase.Date       = convertPV PersistDay
getGetter MySQLBase.NewDate    = convertPV PersistDay
getGetter MySQLBase.Year       = convertPV PersistDay
-- Null
getGetter MySQLBase.Null       = \_ _ -> PersistNull
-- Controversial conversions
getGetter MySQLBase.Set        = convertPV PersistString
getGetter MySQLBase.Enum       = convertPV PersistString
-- Unsupported
getGetter other = error $ "MySQL.getGetter: type " ++
                  show other ++ " not supported."

proxy :: Proxy MySQL
proxy = error "Proxy MySQL"

noLimit :: Utf8
noLimit = "LIMIT 18446744073709551615"

withSchema :: Maybe String -> String -> String
withSchema sch name = maybe "" (\x -> escape x ++ ".") sch ++ escape name
