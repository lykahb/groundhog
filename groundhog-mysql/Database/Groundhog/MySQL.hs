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

import Control.Applicative ((<|>))
import Control.Arrow (first, second, (***))
import Control.Monad (liftM, liftM2, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State (mapStateT)
import Data.ByteString.Char8 (ByteString)
import Data.Char (toUpper)
import Data.Function (on)
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (groupBy, intercalate, intersect, isInfixOf, partition, stripPrefix)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import Data.Pool

newtype MySQL = MySQL MySQL.Connection

instance DbDescriptor MySQL where
  type AutoKeyType MySQL = Int64
  type QueryRaw MySQL = Snippet MySQL
  backendName _ = "mysql"

instance SqlDb MySQL where
  append a b = mkExpr $ function "concat" [toExpr a, toExpr b]
  signum' a = mkExpr $ function "sign" [toExpr a]
  quotRem' x y = (mkExpr $ operator 70 " div " x y, mkExpr $ operator 70 " % " x y)
  equalsOperator a b = a <> "<=>" <> b
  notEqualsOperator a b = "NOT(" <> a <> "<=>" <> b <> ")"

instance FloatingSqlDb MySQL where
  log' a = mkExpr $ function "log" [toExpr a]
  logBase' b x = mkExpr $ function "log" [toExpr b, toExpr x]

instance PersistBackendConn MySQL where
  insert v = runDb' $ insert' v
  insert_ v = runDb' $ insert_' v
  insertBy u v = runDb' $ H.insertBy renderConfig queryRaw' True u v
  insertByAll v = runDb' $ H.insertByAll renderConfig queryRaw' True v
  replace k v = runDb' $ H.replace renderConfig queryRaw' executeRaw' insertIntoConstructorTable k v
  replaceBy k v = runDb' $ H.replaceBy renderConfig executeRaw' k v
  select options = runDb' $ H.select renderConfig queryRaw' preColumns noLimit options
  selectStream options = runDb' $ H.selectStream renderConfig queryRaw' preColumns noLimit options
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
  project p options = runDb' $ H.project renderConfig queryRaw' preColumns noLimit p options
  projectStream p options = runDb' $ H.projectStream renderConfig queryRaw' preColumns noLimit p options
  migrate fakeV = mapStateT runDb' $ migrate' fakeV

  executeRaw _ query ps = runDb' $ executeRaw' (fromString query) ps
  queryRaw _ query ps = runDb' $ queryRaw' (fromString query) ps

  insertList l = runDb' $ insertList' l
  getList k = runDb' $ getList' k

instance SchemaAnalyzer MySQL where
  schemaExists schema = runDb' $ queryRaw' "SELECT 1 FROM information_schema.schemata WHERE schema_name=?" [toPrimitivePersistValue proxy schema] >>= firstRow >>= return . isJust
  getCurrentSchema = runDb' $ queryRaw' "SELECT database()" [] >>= firstRow >>= return . (>>= fst . fromPurePersistValues proxy)
  listTables schema = runDb' $ queryRaw' "SELECT table_name FROM information_schema.tables WHERE table_schema=coalesce(?,database())" [toPrimitivePersistValue proxy schema] >>= mapStream (return . fst . fromPurePersistValues proxy) >>= streamToList
  listTableTriggers name = runDb' $ queryRaw' "SELECT trigger_name FROM information_schema.triggers WHERE event_object_schema=coalesce(?,database()) AND event_object_table=?" (toPurePersistValues proxy name []) >>= mapStream (return . fst . fromPurePersistValues proxy) >>= streamToList
  analyzeTable = runDb' . analyzeTable'
  analyzeTrigger name = runDb' $ do
    x <- queryRaw' "SELECT action_statement FROM information_schema.triggers WHERE trigger_schema=coalesce(?,database()) AND trigger_name=?" (toPurePersistValues proxy name []) >>= firstRow
    return $ case x of
      Nothing  -> Nothing
      Just src -> fst $ fromPurePersistValues proxy src
  analyzeFunction name = runDb' $ do
    result <- queryRaw' "SELECT param_list, returns, body_utf8 from mysql.proc WHERE db = coalesce(?, database()) AND name = ?" (toPurePersistValues proxy name []) >>= firstRow
    let read' typ = readSqlType typ typ (Nothing, Nothing, Nothing)
    return $ case result of
      Nothing  -> Nothing
      -- TODO: parse param_list
      Just result' -> Just (Just [DbOther (OtherTypeDef [Left param_list])], if ret == "" then Nothing else Just $ read' ret, src) where
        (param_list, ret, src) = fst . fromPurePersistValues proxy $ result'
  getMigrationPack = fmap (migrationPack . fromJust) getCurrentSchema

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

instance ConnectionManager MySQL where
  withConn f conn@(MySQL c) = do
    liftIO $ MySQL.execute_ c "start transaction"
    x <- onException (f conn) (liftIO $ MySQL.rollback c)
    liftIO $ MySQL.commit c
    return x

instance ExtractConnection MySQL MySQL where
  extractConn f conn = f conn

instance ExtractConnection (Pool MySQL) MySQL where
  extractConn f pconn = withResource pconn f

open' :: MySQL.ConnectInfo -> IO MySQL
open' ci = do
  conn <- MySQL.connect ci
  MySQLBase.autocommit conn False -- disable autocommit!
  return $ MySQL conn

close' :: MySQL -> IO ()
close' (MySQL conn) = MySQL.close conn

insert' :: PersistEntity v => v -> Action MySQL (AutoKey v)
insert' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef proxy v
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

insert_' :: PersistEntity v => v -> Action MySQL ()
insert_' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef proxy v
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
  query = "INSERT INTO " <> tName <> columnsValues
  fields = case constrAutoKeyName c of
    Just idName | withId -> (idName, dbType proxy (0 :: Int64)):constrParams c
    _                    -> constrParams c
  columnsValues = case foldr (flatten escapeS) [] fields of
    [] -> "() VALUES ()"
    xs -> "(" <> commasJoin xs <> ") VALUES(" <> placeholders <> ")"
  RenderS placeholders vals' = commasJoin $ map renderPersistValue vals

insertList' :: forall a. PersistField a => [a] -> Action MySQL Int64
insertList' (l :: [a]) = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  executeRaw' ("INSERT INTO " <> escapeS mainName <> "()VALUES()") []
  k <- getLastInsertId
  let valuesName = mainName <> delim' <> "values"
  let fields = [("ord", dbType proxy (0 :: Int)), ("value", dbType proxy (undefined :: a))]
  let query = "INSERT INTO " <> escapeS valuesName <> "(id," <> renderFields escapeS fields <> ")VALUES(?," <> renderFields (const $ fromChar '?') fields <> ")"
  let go :: Int -> [a] -> Action MySQL ()
      go n (x:xs) = do
       x' <- toPersistValues x
       executeRaw' query $ (k:) . (toPrimitivePersistValue proxy n:) . x' $ []
       go (n + 1) xs
      go _ [] = return ()
  go 0 l
  return $ fromPrimitivePersistValue proxy k
  
getList' :: forall a . PersistField a => Int64 -> Action MySQL [a]
getList' k = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  let valuesName = mainName <> delim' <> "values"
  let value = ("value", dbType proxy (undefined :: a))
  let query = "SELECT " <> renderFields escapeS [value] <> " FROM " <> escapeS valuesName <> " WHERE id=? ORDER BY ord"
  queryRaw' query [toPrimitivePersistValue proxy k] >>= mapStream (liftM fst . fromPersistValues) >>= streamToList

getLastInsertId :: Action MySQL PersistValue
getLastInsertId = do
  x <- queryRaw' "SELECT last_insert_id()" [] >>= firstRow
  return $ maybe (error "getLastInsertId: Nothing") head x

----------

executeRaw' :: Utf8 -> [PersistValue] -> Action MySQL ()
executeRaw' query vals = do
--  $logDebugS "SQL" $ fromString $ show (fromUtf8 query) ++ " " ++ show vals
  MySQL conn <- ask
  let stmt = getStatement query
  liftIO $ do
    _ <- MySQL.execute conn stmt (map P vals)
    return ()

renderConfig :: RenderConfig
renderConfig = RenderConfig {
    esc = escapeS
}

escapeS :: Utf8 -> Utf8
escapeS a = let q = fromChar '`' in q <> a <> q

delim' :: Utf8
delim' = fromChar delim

toEntityPersistValues' :: PersistEntity v => v -> Action MySQL [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

--- MIGRATION

migrate' :: PersistEntity v => v -> Migration (Action MySQL)
migrate' v = do
  migPack <- lift getMigrationPack
  migrateRecursively (migrateSchema migPack) (migrateEntity migPack) (migrateList migPack) v

migrationPack :: String -> GM.MigrationPack MySQL
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
    "BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY"
    mainTableId
    defaultPriority
    (\uniques refs -> ([], map AddUnique uniques ++ map AddReference refs))
    showSqlType
    showColumn
    (showAlterDb currentSchema)
    Restrict
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

migTriggerOnDelete :: QualifiedName -> [(String, String)] -> Action MySQL (Bool, [AlterDB])
migTriggerOnDelete name deletes = do
  let addTrigger = AddTriggerOnDelete name name (concatMap snd deletes)
  x <- analyzeTrigger name
  return $ case x of
    Nothing | null deletes -> (False, [])
    Nothing -> (False, [addTrigger])
    Just sql -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
      then [DropTrigger name name]
      else if sql == "BEGIN " ++ concatMap snd deletes ++ "END"
        then []
        -- this can happen when an ephemeral field was added or removed.
        else [DropTrigger name name, addTrigger])

-- | Schema name, table name and a list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: QualifiedName -> [(String, String)] -> Action MySQL [(Bool, [AlterDB])]
migTriggerOnUpdate tName deletes = do
  let trigName = second (++ "_ON_UPDATE") tName
      f (fieldName, del) = "IF NOT (NEW." ++ escape fieldName ++ " <=> OLD." ++ escape fieldName ++ ") THEN " ++ del ++ " END IF;"
      trigBody = concatMap f deletes
  let addTrigger = AddTriggerOnUpdate trigName tName Nothing trigBody
  x <- analyzeTrigger trigName
  return $ return $ case x of
    Nothing | null deletes -> (False, [])
    Nothing -> (False, [addTrigger])
    Just sql -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
      then [DropTrigger trigName tName]
      else if sql == "BEGIN " ++ trigBody ++ "END"
        then []
        -- this can happen when an ephemeral field was added or removed.
        else [DropTrigger trigName tName, addTrigger])
  
analyzeTable' :: QualifiedName -> Action MySQL (Maybe TableInfo)
analyzeTable' name = do
  table <- queryRaw' "SELECT * FROM information_schema.tables WHERE table_schema = coalesce(?, database()) AND table_name = ?" (toPurePersistValues proxy name []) >>= firstRow
  case table of
    Just _ -> do
      let colQuery = "SELECT c.column_name, c.is_nullable, c.data_type, c.column_type, c.column_default, c.character_maximum_length, c.numeric_precision, c.numeric_scale, c.extra\
\  FROM information_schema.columns c\
\  WHERE c.table_schema = coalesce(?, database()) AND c.table_name=?\
\  ORDER BY c.ordinal_position"

      cols <- queryRaw' colQuery (toPurePersistValues proxy name []) >>= mapStream (return . first getColumn . fst . fromPurePersistValues proxy) >>= streamToList
      -- MySQL has no difference between unique keys and indexes
      let constraintQuery = "SELECT u.constraint_name, u.column_name FROM information_schema.table_constraints tc INNER JOIN information_schema.key_column_usage u USING (constraint_catalog, constraint_schema, constraint_name, table_schema, table_name) WHERE tc.constraint_type=? AND tc.table_schema=coalesce(?,database()) AND u.table_name=? ORDER BY u.constraint_name, u.column_name"
      uniqConstraints <- queryRaw' constraintQuery (toPurePersistValues proxy ("UNIQUE" :: String, name) []) >>= mapStream (return . fst . fromPurePersistValues proxy) >>= streamToList
      uniqPrimary <- queryRaw' constraintQuery (toPurePersistValues proxy ("PRIMARY KEY" :: String, name) []) >>= mapStream (return . fst . fromPurePersistValues proxy) >>= streamToList
      let mkUniqs typ = map (\us -> UniqueDef (fst $ head us) typ (map (Left . snd) us)) . groupBy ((==) `on` fst)
          isAutoincremented = case filter (\c -> colName (fst c) `elem` map snd uniqPrimary) cols of
                                [(c, extra)] -> colType c `elem` [DbInt32, DbInt64] && "auto_increment" `isInfixOf` (extra :: String)
                                _ -> False
          uniqs = mkUniqs UniqueConstraint uniqConstraints ++ mkUniqs (UniquePrimary isAutoincremented) uniqPrimary
      references <- analyzeTableReferences name
      return $ Just $ TableInfo (map fst cols) uniqs references
    Nothing -> return Nothing

getColumn :: ((String, String, String, String, Maybe String), (Maybe Int, Maybe Int, Maybe Int)) -> Column
getColumn ((column_name, is_nullable, data_type, column_type, d), modifiers) = Column column_name (is_nullable == "YES") t d where
  t = readSqlType data_type column_type modifiers

analyzeTableReferences :: QualifiedName -> Action MySQL [(Maybe String, Reference)]
analyzeTableReferences name = do
  let query = "SELECT tc.constraint_name, u.referenced_table_schema, u.referenced_table_name, rc.delete_rule, rc.update_rule, u.column_name, u.referenced_column_name FROM information_schema.table_constraints tc\
\  INNER JOIN information_schema.key_column_usage u USING (constraint_catalog, constraint_schema, constraint_name, table_schema, table_name)\
\  INNER JOIN information_schema.referential_constraints rc USING (constraint_catalog, constraint_schema, constraint_name)\
\  WHERE tc.constraint_type='FOREIGN KEY' AND tc.table_schema=coalesce(?, database()) AND tc.table_name=?\
\  ORDER BY tc.constraint_name"
  x <- queryRaw' query (toPurePersistValues proxy name []) >>= mapStream (return . fst . fromPurePersistValues proxy) >>= streamToList
  -- (refName, ((parentTableSchema, parentTable, onDelete, onUpdate), (childColumn, parentColumn)))
  let mkReference xs = (Just refName, Reference parentTable pairs (mkAction onDelete) (mkAction onUpdate)) where
        pairs = map (snd . snd) xs
        (refName, ((parentTable, onDelete, onUpdate), _)) = head xs
        mkAction c = Just $ fromMaybe (error $ "unknown reference action type: " ++ c) $ readReferenceAction c
      references = map mkReference $ groupBy ((==) `on` fst) x
  return references

showAlterDb :: String -> AlterDB -> SingleMigration
showAlterDb _ (AddTable s) = Right [(False, defaultPriority, s)]
showAlterDb currentSchema (AlterTable t _ _ _ alts) = Right $ concatMap (showAlterTable currentSchema $ withSchema t) alts
showAlterDb _ (DropTrigger trigName _) = Right [(False, triggerPriority, "DROP TRIGGER " ++ withSchema trigName)]
showAlterDb _ (AddTriggerOnDelete trigName tName body) = Right [(False, triggerPriority, "CREATE TRIGGER " ++ withSchema trigName ++ " AFTER DELETE ON " ++ withSchema tName ++ " FOR EACH ROW BEGIN " ++ body ++ "END")]
showAlterDb _ (AddTriggerOnUpdate trigName tName _ body) = Right [(False, triggerPriority, "CREATE TRIGGER " ++ withSchema trigName ++ " AFTER UPDATE ON " ++ withSchema tName ++ " FOR EACH ROW BEGIN " ++ body ++ "END")]
showAlterDb _ (CreateOrReplaceFunction s) = Right [(False, functionPriority, s)]
showAlterDb _ (DropFunction funcName) = Right [(False, functionPriority, "DROP FUNCTION " ++ withSchema funcName ++ "()")]
showAlterDb _ (CreateSchema sch ifNotExists) = Right [(False, schemaPriority, "CREATE DATABASE " ++ ifNotExists' ++ escape sch)] where
  ifNotExists' = if ifNotExists then "IF NOT EXISTS " else ""

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
showAlterTable _ table (AddUnique (UniqueDef uName UniqueConstraint cols)) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD"
  , maybe "" ((" CONSTRAINT " ++) . escape) uName
  , " UNIQUE("
  , intercalate "," $ map (either escape id) cols
  , ")"
  ])]
showAlterTable _ table (AddUnique (UniqueDef uName UniqueIndex cols)) = [(False, defaultPriority, concat
  [ "CREATE UNIQUE INDEX "
  , maybe (error $ "showAlterTable: index for table " ++ table ++ " does not have a name") escape uName
  , " ON "
  , table
  , "("
  , intercalate "," $ map (either escape id) cols
  , ")"
  ])]
showAlterTable _ table (AddUnique (UniqueDef uName (UniquePrimary _) cols)) = [(False, defaultPriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD"
  , maybe "" ((" CONSTRAINT " ++) . escape) uName
  , " PRIMARY KEY("
  , intercalate "," $ map (either escape id) cols
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
showAlterTable currentSchema table (AddReference (Reference tName columns onDelete onUpdate)) = [(False, referencePriority, concat
  [ "ALTER TABLE "
  , table
  , " ADD FOREIGN KEY("
  , our
  , ") REFERENCES "
  , withSchema $ first (<|> Just currentSchema) tName
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
  _ -> DbOther $ OtherTypeDef [Left colTyp]
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
  DbOther (OtherTypeDef ts) -> concatMap (either id showSqlType) ts

compareUniqs :: UniqueDefInfo -> UniqueDefInfo -> Bool
compareUniqs (UniqueDef _ (UniquePrimary _) cols1) (UniqueDef _ (UniquePrimary _) cols2) = haveSameElems (==) cols1 cols2
compareUniqs (UniqueDef name1 _ cols1) (UniqueDef name2 _ cols2) = fromMaybe True (liftM2 (==) name1 name2) && haveSameElems (==) cols1 cols2

compareRefs :: String -> (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
compareRefs currentSchema (_, Reference (sch1, tbl1) pairs1 onDel1 onUpd1) (_, Reference (sch2, tbl2) pairs2 onDel2 onUpd2) =
     fromMaybe currentSchema sch1 == fromMaybe currentSchema sch2
  && unescape tbl1 == unescape tbl2
  && haveSameElems (==) pairs1 pairs2
  && fromMaybe Restrict onDel1 == fromMaybe Restrict onDel2
  && fromMaybe Restrict onUpd1 == fromMaybe Restrict onUpd2 where
    unescape name = if head name == '"' && last name == '"' then tail $ init name else name

compareTypes :: DbTypePrimitive -> DbTypePrimitive -> Bool
compareTypes type1 type2 = f type1 == f type2 where
  f = map toUpper . showSqlType . hack
  hack DbDayTimeZoned = DbOther $ OtherTypeDef [Left "VARCHAR(50)"]
  hack t = t

compareDefaults :: String -> String -> Bool
compareDefaults def1 def2 = not . null $ f def1 `intersect` f def2 where
  f def = [Just def, stripQuotes def]
  stripQuotes = stripPrefix "'" >=> fmap reverse . stripPrefix "'" . reverse

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
escape s = '`' : s ++ "`"
  
getStatement :: Utf8 -> MySQL.Query
getStatement sql = MySQL.Query $ fromUtf8 sql

queryRaw' :: Utf8 -> [PersistValue] -> Action MySQL (RowStream [PersistValue])
queryRaw' query vals = do
--  $logDebugS "SQL" $ fromString $ show (fromUtf8 query) ++ " " ++ show vals
  MySQL conn <- ask
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
  let next = do
        rows <- liftIO $ readIORef rowsVar
        case rows of
          [] -> return Nothing
          (x:xs) -> do
            liftIO $ writeIORef rowsVar xs
            return $ Just x
  return (next, Nothing)

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

proxy :: proxy MySQL
proxy = error "proxy MySQL"

noLimit :: Utf8
noLimit = "LIMIT 18446744073709551615"

withSchema :: QualifiedName -> String
withSchema (sch, name) = maybe "" (\x -> escape x ++ ".") sch ++ escape name

preColumns :: HasSelectOptions opts db r => opts -> RenderS db r
preColumns _ = mempty
