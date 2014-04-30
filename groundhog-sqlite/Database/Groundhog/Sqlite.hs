{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies, RecordWildCards, Rank2Types, MultiParamTypeClasses, TemplateHaskell #-}
module Database.Groundhog.Sqlite
    ( withSqlitePool
    , withSqliteConn
    , createSqlitePool
    , runDbConn
    , Sqlite(..)
    , module Database.Groundhog
    , module Database.Groundhog.Generic.Sql.Functions
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

import qualified Database.SQLite3 as S
import qualified Database.SQLite3.Direct as SD

import Control.Arrow ((***))
import Control.Monad (liftM, forM)
import Control.Monad.Logger (MonadLogger, NoLoggingT, logDebugS)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toUpper)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (groupBy, intercalate, isInfixOf, partition, sort)
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Pool
import qualified Data.Text as T

-- typical operations for connection: OPEN, BEGIN, COMMIT, ROLLBACK, CLOSE
data Sqlite = Sqlite S.Database (IORef (Map.HashMap BS.ByteString S.Statement))

instance DbDescriptor Sqlite where
  type AutoKeyType Sqlite = Int64
  type QueryRaw Sqlite = Snippet Sqlite
  backendName _ = "sqlite"

instance SqlDb Sqlite where
  append a b = mkExpr $ operator 50 "||" a b
  signum' x = mkExpr $ Snippet $ \esc _ -> let
       x' = renderExpr esc (toExpr x)
    in ["case when (" <> x' <> ") > 0 then 1 when (" <> x' <> ") < 0 then -1 else 0 end"]
  quotRem' x y = (mkExpr $ operator 70 "/" x y, mkExpr $ operator 70 "%" x y)
  equalsOperator a b = a <> " IS " <> b
  notEqualsOperator a b = a <> " IS NOT " <> b

instance (MonadBaseControl IO m, MonadIO m, MonadLogger m) => PersistBackend (DbPersist Sqlite m) where
  {-# SPECIALIZE instance PersistBackend (DbPersist Sqlite (NoLoggingT IO)) #-}
  type PhantomDb (DbPersist Sqlite m) = Sqlite
  insert v = insert' v
  insert_ v = insert_' v
  insertBy u v = H.insertBy renderConfig queryRawCached' True u v
  insertByAll v = H.insertByAll renderConfig queryRawCached' True v
  replace k v = H.replace renderConfig queryRawCached' executeRawCached' insertIntoConstructorTable k v
  replaceBy k v = H.replaceBy renderConfig executeRawCached' k v
  select options = H.select renderConfig queryRawCached' "LIMIT -1"  options
  selectAll = H.selectAll renderConfig queryRawCached'
  get k = H.get renderConfig queryRawCached' k
  getBy k = H.getBy renderConfig queryRawCached' k
  update upds cond = H.update renderConfig executeRawCached' upds cond
  delete cond = H.delete renderConfig executeRawCached' cond
  deleteBy k = H.deleteBy renderConfig executeRawCached' k
  deleteAll v = H.deleteAll renderConfig executeRawCached' v
  count cond = H.count renderConfig queryRawCached' cond
  countAll fakeV = H.countAll renderConfig queryRawCached' fakeV
  project p options = H.project renderConfig queryRawCached' "LIMIT -1" p options
  migrate fakeV = migrate' fakeV

  executeRaw False query ps = executeRaw' (fromString query) ps
  executeRaw True query ps = executeRawCached' (fromString query) ps
  queryRaw False query ps f = queryRaw' (fromString query) ps f
  queryRaw True query ps f = queryRawCached' (fromString query) ps f

  insertList l = insertList' l
  getList k = getList' k

instance (MonadBaseControl IO m, MonadIO m, MonadLogger m) => SchemaAnalyzer (DbPersist Sqlite m) where
  schemaExists = error "schemaExists: is not supported by Sqlite"
  listTables _ = queryRaw' "SELECT name FROM sqlite_master WHERE type='table'" [] (mapAllRows $ return . fst . fromPurePersistValues proxy)
  listTableTriggers _ name = queryRaw' "SELECT name FROM sqlite_master WHERE type='trigger' AND tbl_name=?" [toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
  analyzeTable = analyzeTable'
  analyzeTrigger _ name = do
    x <- queryRaw' "SELECT sql FROM sqlite_master WHERE type='trigger' AND name=?" [toPrimitivePersistValue proxy name] id
    case x of
      Nothing  -> return Nothing
      Just src -> return (fst $ fromPurePersistValues proxy src)
  analyzeFunction = error "analyzeFunction: is not supported by Sqlite"

withSqlitePool :: (MonadBaseControl IO m, MonadIO m)
               => String -- ^ connection string
               -> Int -- ^ number of connections to open
               -> (Pool Sqlite -> m a)
               -> m a
withSqlitePool s connCount f = createSqlitePool s connCount >>= f

withSqliteConn :: (MonadBaseControl IO m, MonadIO m)
               => String -- ^ connection string
               -> (Sqlite -> m a)
               -> m a
withSqliteConn s = bracket (liftIO $ open' s) (liftIO . close')

createSqlitePool :: MonadIO m
                 => String -- ^ connection string
                 -> Int -- ^ number of connections to open
                 -> m (Pool Sqlite)
createSqlitePool s connCount = liftIO $ createPool (open' s) close' 1 20 connCount

instance Savepoint Sqlite where
  withConnSavepoint name m (Sqlite c _) = do
    let name' = fromString name
    liftIO $ S.exec c $ "SAVEPOINT " <> name'
    x <- onException m (liftIO $ S.exec c $ "ROLLBACK TO " <> name')
    liftIO $ S.exec c $ "RELEASE " <> name'
    return x

instance ConnectionManager Sqlite Sqlite where
  withConn f conn@(Sqlite c _) = do
    liftIO $ S.exec c "BEGIN"
    x <- onException (f conn) (liftIO $ S.exec c "ROLLBACK")
    liftIO $ S.exec c "COMMIT"
    return x
  withConnNoTransaction f conn = f conn

instance ConnectionManager (Pool Sqlite) Sqlite where
  withConn f pconn = withResource pconn (withConn f)
  withConnNoTransaction f pconn = withResource pconn (withConnNoTransaction f)

instance SingleConnectionManager Sqlite Sqlite

open' :: String -> IO Sqlite
open' s = do
  conn <- S.open $ T.pack s
  S.prepare conn "PRAGMA foreign_keys = ON" >>= \stmt -> S.step stmt >> S.finalize stmt
  cache <- newIORef Map.empty
  return $ Sqlite conn cache

close' :: Sqlite -> IO ()
close' (Sqlite conn smap) = do
  readIORef smap >>= mapM_ S.finalize . Map.elems
  S.close conn

migrate' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m) => v -> Migration (DbPersist Sqlite m)
migrate' = migrateRecursively (const $ return $ Right []) (migrateEntity migrationPack) (migrateList migrationPack)

migrationPack :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => GM.MigrationPack (DbPersist Sqlite m)
migrationPack = GM.MigrationPack
  compareTypes
  compareRefs
  compareUniqs
  compareDefaults
  migTriggerOnDelete
  migTriggerOnUpdate
  GM.defaultMigConstr
  escape
  "INTEGER PRIMARY KEY NOT NULL"
  mainTableId
  defaultPriority
  addUniquesReferences
  showColumn
  showAlterDb
  NoAction

addUniquesReferences :: [UniqueDef'] -> [Reference] -> ([String], [AlterTable])
addUniquesReferences uniques refs = (map sqlUnique constraints ++ map sqlReference refs, map AddUnique indexes) where
  (constraints, indexes) = partition ((/= UniqueIndex) . uniqueDefType) uniques

migTriggerOnDelete :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Maybe String -> String -> [(String, String)] -> DbPersist Sqlite m (Bool, [AlterDB])
migTriggerOnDelete schema name deletes = do
  let addTrigger = AddTriggerOnDelete Nothing name Nothing name (concatMap snd deletes)
  x <- analyzeTrigger schema name
  return $ case x of
    Nothing | null deletes -> (False, [])
    Nothing -> (False, [addTrigger])
    Just sql -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
      then [DropTrigger Nothing name Nothing name]
      else if Right [(False, triggerPriority, sql)] == showAlterDb addTrigger
        then []
        -- this can happen when an ephemeral field was added or removed.
        else [DropTrigger Nothing name Nothing name, addTrigger])
        
-- | Schema name, table name and a list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Maybe String -> String -> [(String, String)] -> DbPersist Sqlite m [(Bool, [AlterDB])]
migTriggerOnUpdate schema name dels = forM dels $ \(fieldName, del) -> do
  let trigName = name ++ delim : fieldName
  let addTrigger = AddTriggerOnUpdate Nothing trigName Nothing name (Just fieldName) del
  x <- analyzeTrigger schema trigName
  return $ case x of
    Nothing -> (False, [addTrigger])
    Just sql -> (True, if Right [(False, triggerPriority, sql)] == showAlterDb addTrigger
        then []
        else [DropTrigger Nothing trigName Nothing name, addTrigger])

analyzeTable' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Maybe String -> String -> DbPersist Sqlite m (Maybe TableInfo)
analyzeTable' _ tName = do
  let fromName = escapeS . fromString
  tableInfo <- queryRaw' ("pragma table_info(" <> fromName tName <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  case tableInfo of
    [] -> return Nothing
    rawColumns -> do
      let mkColumn :: (Int, (String, String, Int, Maybe String, Int)) -> Column
          mkColumn (_, (name, typ, isNotNull, defaultValue, _)) = Column name (isNotNull == 0) (readSqlType typ) defaultValue
          primaryKeyColumnNames = foldr (\(_ , (name, _, _, _, primaryIndex)) xs -> if primaryIndex > 0 then name:xs else xs) [] rawColumns
          columns = map mkColumn rawColumns
      indexList <- queryRaw' ("pragma index_list(" <> fromName tName <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
      let uniqueNames = map (\(_ :: Int, name, _) -> name) $ filter (\(_, _, isUnique) -> isUnique) indexList
      uniques <- forM uniqueNames $ \name -> do
        uFields <- queryRaw' ("pragma index_info(" <> fromName name <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
        sql <- queryRaw' ("select sql from sqlite_master where type = 'index' and name = ?") [toPrimitivePersistValue proxy name] id
        let columnNames = map (\(_, _, columnName) -> columnName) (uFields :: [(Int, Int, String)])
            uType = if sql == Just [PersistNull]
              then if sort columnNames == sort primaryKeyColumnNames then UniquePrimary False else UniqueConstraint
              else UniqueIndex
        return $ UniqueDef' (Just name) uType columnNames
      foreignKeyList <- queryRaw' ("pragma foreign_key_list(" <> fromName tName <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
      (foreigns :: [(Maybe String, Reference)]) <- do
          let foreigns :: [[(Int, (Int, String, (String, Maybe String), (String, String, String)))]]
              foreigns = groupBy ((==) `on` fst) . sort $ foreignKeyList -- sort by foreign key number and column number inside key (first and second integers)
              mkAction c = Just $ fromMaybe (error $ "unknown reference action type: " ++ c) $ readReferenceAction c
          forM foreigns $ \rows -> do 
            let (_, (_, foreignTable, _, (onUpdate, onDelete, _))) = head rows
                (children, parents) = unzip $ map (\(_, (_, _, pair, _)) -> pair) rows
            parents' <- case head parents of
              Nothing -> analyzePrimaryKey foreignTable >>= \x -> case x of
                Just primaryCols -> return primaryCols
                Nothing -> error $ "analyzeTable: cannot find primary key for table " ++ foreignTable ++ " which is referenced without specifying column names"
              Just _ -> return $ map (fromMaybe (error "analyzeTable: all parents must be either NULL or values")) parents
            let refs = zip children parents'
            return (Nothing, Reference Nothing foreignTable refs (mkAction onDelete) (mkAction onUpdate))
      let notPrimary x = case x of
            UniquePrimary _ -> False
            _ -> True
          uniques' = uniques ++ 
            if all (notPrimary . uniqueDefType) uniques && not (null primaryKeyColumnNames)
              then  [UniqueDef' Nothing (UniquePrimary True) primaryKeyColumnNames]
              else []
      return $ Just $ TableInfo columns uniques' foreigns

analyzePrimaryKey :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => String -> DbPersist Sqlite m (Maybe [String])
analyzePrimaryKey tName = do
  tableInfo <- queryRaw' ("pragma table_info(" <> escapeS (fromString tName) <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  let cols = map (\(_ , (name, _, _, _, primaryIndex)) -> (primaryIndex, name)) (tableInfo ::  [(Int, (String, String, Int, Maybe String, Int))])
      cols' = map snd $ sort $ filter ((> 0) . fst) cols
  return $ if null cols'
    then Nothing
    else Just cols'

getStatementCached :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> DbPersist Sqlite m S.Statement
getStatementCached sql = do
  Sqlite conn smap <- DbPersist ask
  liftIO $ do
    smap' <- readIORef smap
    let sql' = fromUtf8 sql
    case Map.lookup sql' smap' of
      Nothing -> do
        stmt <- S.prepareUtf8 conn $ SD.Utf8 sql'
        writeIORef smap (Map.insert sql' stmt smap')
        return stmt
      Just stmt -> return stmt

getStatement :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> DbPersist Sqlite m S.Statement
getStatement sql = do
  Sqlite conn _ <- DbPersist ask
  liftIO $ S.prepareUtf8 conn $ SD.Utf8 $ fromUtf8 sql

showSqlType :: DbTypePrimitive -> String
showSqlType t = case t of
  DbString -> "VARCHAR"
  DbInt32 -> "INTEGER"
  DbInt64 -> "INTEGER"
  DbReal -> "REAL"
  DbBool -> "BOOLEAN"
  DbDay -> "DATE"
  DbTime -> "TIME"
  DbDayTime -> "TIMESTAMP"
  DbDayTimeZoned -> "TIMESTAMP WITH TIME ZONE"
  DbBlob -> "BLOB"
  DbAutoKey -> "INTEGER"
  DbOther (OtherTypeDef f) -> f showSqlType

readSqlType :: String -> DbTypePrimitive
readSqlType "VARCHAR" = DbString
readSqlType "INTEGER" = DbInt64
readSqlType "REAL" = DbReal
readSqlType "BOOLEAN" = DbBool
readSqlType "DATE" = DbDay
readSqlType "TIME" = DbTime
readSqlType "TIMESTAMP" = DbDayTime
readSqlType "TIMESTAMP WITH TIME ZONE" = DbDayTimeZoned
readSqlType "BLOB" = DbBlob
readSqlType typ = DbOther $ OtherTypeDef $ const typ

data Affinity = TEXT | NUMERIC | INTEGER | REAL | NONE deriving (Eq, Show)

dbTypeAffinity :: DbTypePrimitive -> Affinity
dbTypeAffinity = readSqlTypeAffinity . showSqlType

readSqlTypeAffinity :: String -> Affinity
readSqlTypeAffinity typ = affinity where
  contains = any (`isInfixOf` map toUpper typ)
  affinity = case () of
    _ | contains ["INT"] -> INTEGER
    _ | contains ["CHAR", "CLOB", "TEXT"]  -> TEXT
    _ | contains ["BLOB"] || null typ -> NONE
    _ | contains ["REAL", "FLOA", "DOUB"]  -> REAL
    _ -> NUMERIC

showColumn :: Column -> String
showColumn (Column name nullable typ def) = escape name ++ " " ++ showSqlType typ ++ rest where
  rest = concat [
    if not nullable then " NOT NULL" else "",
    maybe "" (" DEFAULT " ++) def]

sqlReference :: Reference -> String
sqlReference Reference{..} = "FOREIGN KEY(" ++ our ++ ") REFERENCES " ++ escape referencedTableName ++ "(" ++ foreign ++ ")" ++ actions where
  actions = maybe "" ((" ON DELETE " ++) . showReferenceAction) referenceOnDelete
         ++ maybe "" ((" ON UPDATE " ++) . showReferenceAction) referenceOnUpdate
  (our, foreign) = f *** f $ unzip referencedColumns
  f = intercalate ", " . map escape

sqlUnique :: UniqueDef' -> String
sqlUnique (UniqueDef' name typ cols) = concat [
    maybe "" (\x -> "CONSTRAINT " ++ escape x ++ " ") name
  , constraintType
  , intercalate "," $ map escape cols
  , ")"
  ] where
    constraintType = case typ of
      UniquePrimary _ -> "PRIMARY KEY("
      UniqueConstraint -> "UNIQUE("
      UniqueIndex -> error "sqlUnique: does not handle indexes"

insert' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m) => v -> DbPersist Sqlite m (AutoKey v)
insert' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef v
  let constructorNum = fromPrimitivePersistValue proxy (head vals)

  liftM fst $ if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let RenderS query vals' = insertIntoConstructorTable False (tableName escapeS e constr) constr (tail vals)
      executeRawCached' query (vals' [])
      case constrAutoKeyName constr of
        Nothing -> pureFromPersistValue []
        Just _  -> getLastInsertRowId >>= \rowid -> pureFromPersistValue [rowid]
    else do
      let constr = constructors e !! constructorNum
      let query = "INSERT INTO " <> mainTableName escapeS e <> "(discr)VALUES(?)"
      executeRawCached' query $ take 1 vals
      rowid <- getLastInsertRowId
      let RenderS cQuery vals' = insertIntoConstructorTable True (tableName escapeS e constr) constr (rowid:tail vals)
      executeRawCached' cQuery (vals' [])
      pureFromPersistValue [rowid]

insert_' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m, MonadLogger m) => v -> DbPersist Sqlite m ()
insert_' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef v
  let constructorNum = fromPrimitivePersistValue proxy (head vals)

  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let RenderS query vals' = insertIntoConstructorTable False (tableName escapeS e constr) constr (tail vals)
      executeRawCached' query (vals' [])
    else do
      let constr = constructors e !! constructorNum
      let query = "INSERT INTO " <> mainTableName escapeS e <> "(discr)VALUES(?)"
      executeRawCached' query $ take 1 vals
      rowid <- getLastInsertRowId
      let RenderS cQuery vals' = insertIntoConstructorTable True (tableName escapeS e constr) constr (rowid:tail vals)
      executeRawCached' cQuery (vals' [])

-- TODO: In Sqlite we can insert null to the id column. If so, id will be generated automatically. Check performance change from this.
insertIntoConstructorTable :: Bool -> Utf8 -> ConstructorDef -> [PersistValue] -> RenderS db r
insertIntoConstructorTable withId tName c vals = RenderS query vals' where
  query = "INSERT INTO " <> tName <> "(" <> fieldNames <> ")VALUES(" <> placeholders <> ")"
  fields = case constrAutoKeyName c of
    Just idName | withId -> (idName, dbType (0 :: Int64)):constrParams c
    _                    -> constrParams c
  fieldNames   = renderFields escapeS fields
  RenderS placeholders vals' = commasJoin $ map renderPersistValue vals

insertList' :: forall m a.(MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistField a) => [a] -> DbPersist Sqlite m Int64
insertList' l = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  executeRawCached' ("INSERT INTO " <> escapeS mainName <> " DEFAULT VALUES") []
  k <- getLastInsertRowId
  let valuesName = mainName <> delim' <> "values"
  let fields = [("ord", dbType (0 :: Int)), ("value", dbType (undefined :: a))]
  let query = "INSERT INTO " <> escapeS valuesName <> "(id," <> renderFields escapeS fields <> ")VALUES(?," <> renderFields (const $ fromChar '?') fields <> ")"
  let go :: Int -> [a] -> DbPersist Sqlite m ()
      go n (x:xs) = do
       x' <- toPersistValues x
       executeRawCached' query $ (k:) . (toPrimitivePersistValue proxy n:) . x' $ []
       go (n + 1) xs
      go _ [] = return ()
  go 0 l
  return $ fromPrimitivePersistValue proxy k
  
getList' :: forall m a.(MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistField a) => Int64 -> DbPersist Sqlite m [a]
getList' k = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  let valuesName = mainName <> delim' <> "values"
  let value = ("value", dbType (undefined :: a))
  let query = "SELECT " <> renderFields escapeS [value] <> " FROM " <> escapeS valuesName <> " WHERE id=? ORDER BY ord"
  queryRawCached' query [toPrimitivePersistValue proxy k] $ mapAllRows (liftM fst . fromPersistValues)
    
getLastInsertRowId :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist Sqlite m PersistValue
getLastInsertRowId = do
  Sqlite conn _ <- DbPersist ask
  liftIO (SD.lastInsertRowId conn) >>= toSinglePersistValue

----------

bind :: S.Statement -> [PersistValue] -> IO ()
bind stmt = go 1 where
  go _ [] = return ()
  go i (x:xs) = do
    case x of
      PersistInt64 int64      -> S.bindInt64 stmt i int64
      PersistString text      -> S.bindText stmt i $ T.pack text
      PersistDouble double    -> S.bindDouble stmt i double
      PersistBool b           -> S.bindInt64 stmt i $ if b then 1 else 0
      PersistByteString blob  -> S.bindBlob stmt i blob
      PersistNull             -> S.bindNull stmt i
      PersistDay d            -> S.bindText stmt i $ T.pack $ show d
      PersistTimeOfDay d      -> S.bindText stmt i $ T.pack $ show d
      PersistUTCTime d        -> S.bindText stmt i $ T.pack $ show d
      PersistZonedTime (ZT d) -> S.bindText stmt i $ T.pack $ show d
      PersistCustom _ _       -> error "bind: unexpected PersistCustom"
    go (i + 1) xs

executeRaw' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Utf8 -> [PersistValue] -> DbPersist Sqlite m ()
executeRaw' query vals = do
  $logDebugS "SQL" $ T.pack $ show (fromUtf8 query) ++ " " ++ show vals
  stmt <- getStatement query
  liftIO $ flip finally (S.finalize stmt) $ do
    bind stmt vals
    S.Done <- S.step stmt
    return ()

executeRawCached' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Utf8 -> [PersistValue] -> DbPersist Sqlite m ()
executeRawCached' query vals = do
  $logDebugS "SQL" $ T.pack $ show (fromUtf8 query) ++ " " ++ show vals
  stmt <- getStatementCached query
  liftIO $ flip finally (S.reset stmt) $ do
    bind stmt vals
    S.Done <- S.step stmt
    return ()

queryRaw' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Utf8 -> [PersistValue] -> (RowPopper (DbPersist Sqlite m) -> DbPersist Sqlite m a) -> DbPersist Sqlite m a
queryRaw' query vals f = do
  $logDebugS "SQL" $ T.pack $ show (fromUtf8 query) ++ " " ++ show vals
  stmt <- getStatement query
  flip finally (liftIO $ S.finalize stmt) $ do
    liftIO $ bind stmt vals
    f $ liftIO $ do
      x <- S.step stmt
      case x of
        S.Done -> return Nothing
        S.Row  -> fmap (Just . map pFromSql) $ S.columns stmt

queryRawCached' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Utf8 -> [PersistValue] -> (RowPopper (DbPersist Sqlite m) -> DbPersist Sqlite m a) -> DbPersist Sqlite m a
queryRawCached' query vals f = do
  $logDebugS "SQL" $ T.pack $ show (fromUtf8 query) ++ " " ++ show vals
  stmt <- getStatementCached query
  flip finally (liftIO $ S.reset stmt) $ do
    liftIO $ bind stmt vals
    f $ liftIO $ do
      x <- S.step stmt
      case x of
        S.Done -> return Nothing
        S.Row  -> fmap (Just . map pFromSql) $ S.columns stmt

typeToSqlite :: DbType -> Maybe S.ColumnType
typeToSqlite (DbTypePrimitive t nullable _ _) = case t of
  _ | nullable -> Nothing
  DbOther _ -> Nothing
  DbString -> Just S.TextColumn
  DbInt32 -> Just S.IntegerColumn
  DbInt64 -> Just S.IntegerColumn
  DbReal -> Just S.FloatColumn
  DbBool -> Just S.IntegerColumn
  DbDay -> Just S.TextColumn
  DbTime -> Just S.TextColumn
  DbDayTime -> Just S.TextColumn
  DbDayTimeZoned -> Just S.TextColumn
  DbBlob -> Just S.BlobColumn
  DbAutoKey -> Just S.IntegerColumn
typeToSqlite (DbList _ _) = Just S.IntegerColumn
typeToSqlite t@(DbEmbedded _ _) = error $ "typeToSqlite: DbType does not have corresponding database type: " ++ show t

getDbTypes :: DbType -> [DbType] -> [DbType]
getDbTypes typ acc = case typ of
  DbEmbedded (EmbeddedDef _ ts) _ -> foldr (getDbTypes . snd) acc ts
  t               -> t:acc

pFromSql :: S.SQLData -> PersistValue
pFromSql (S.SQLInteger i) = PersistInt64 i
pFromSql (S.SQLFloat i)   = PersistDouble i
pFromSql (S.SQLText s)    = PersistString (T.unpack s)
pFromSql (S.SQLBlob bs)   = PersistByteString bs
pFromSql (S.SQLNull)      = PersistNull

-- It is used to escape table names and columns, which can include only symbols allowed in Haskell datatypes and '$' delimiter. We need it mostly to support names that coincide with SQL keywords
escape :: String -> String
escape s = '\"' : s ++ "\""

escapeS :: Utf8 -> Utf8
escapeS a = let q = fromChar '"' in q <> a <> q

renderConfig :: RenderConfig
renderConfig = RenderConfig {
    esc = escapeS
}

defaultPriority, triggerPriority :: Int
defaultPriority = 0
triggerPriority = 1

proxy :: proxy Sqlite
proxy = error "proxy Sqlite"

delim' :: Utf8
delim' = fromChar delim

toEntityPersistValues' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, PersistEntity v) => v -> DbPersist Sqlite m [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

compareUniqs :: UniqueDef' -> UniqueDef' -> Bool
compareUniqs (UniqueDef' _ (UniquePrimary _) cols1) (UniqueDef' _ (UniquePrimary _) cols2) = haveSameElems (==) cols1 cols2
compareUniqs (UniqueDef' _ type1 cols1) (UniqueDef' _ type2 cols2) = haveSameElems (==) cols1 cols2 && type1 == type2

compareRefs :: (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
compareRefs (_, Reference _ tbl1 pairs1 onDel1 onUpd1) (_, Reference _ tbl2 pairs2 onDel2 onUpd2) =
     unescape tbl1 == unescape tbl2
  && haveSameElems (==) pairs1 pairs2
  && fromMaybe NoAction onDel1 == fromMaybe NoAction onDel2
  && fromMaybe NoAction onUpd1 == fromMaybe NoAction onUpd2 where
  unescape name = if head name == '"' && last name == '"' then tail $ init name else name

compareTypes :: DbTypePrimitive -> DbTypePrimitive -> Bool
compareTypes type1 type2 = dbTypeAffinity type1 == dbTypeAffinity type2
--compareTypes type1 type2 = showSqlType type1 == showSqlType type2

compareDefaults :: String -> String -> Bool
compareDefaults = (==)

mainTableId :: String
mainTableId = "id"

showAlterDb :: AlterDB -> SingleMigration
showAlterDb (AddTable s) = Right [(False, defaultPriority, s)]
showAlterDb (AlterTable _ table createTable (TableInfo oldCols _ _) (TableInfo newCols _ _) alts) = case mapM (showAlterTable table) alts of
  Just alts' -> Right alts'
  Nothing -> (Right
    [ (False, defaultPriority, "CREATE TEMP TABLE " ++ escape tableTmp ++ "(" ++ columnsTmp ++ ")")
    , (False, defaultPriority, copy (table, columnsTmp) (tableTmp, columnsTmp))
    , (not (null oldOnlyColumns), defaultPriority, "DROP TABLE " ++ escape table)
    , (False, defaultPriority, createTable)
    , (False, defaultPriority, copy (tableTmp, columnsTmp) (table, columnsNew))
    , (False, defaultPriority, "DROP TABLE " ++ escape tableTmp)
    ]) where
      tableTmp = table ++ "_backup"
      copy (from, fromCols) (to, toCols) = "INSERT INTO " ++ escape to ++ "(" ++ toCols ++ ") SELECT " ++ fromCols ++ " FROM " ++ escape from
      (oldOnlyColumns, _, commonColumns) = matchElements ((==) `on` colName) oldCols newCols
      columnsTmp = intercalate "," $ map (escape . colName . snd) commonColumns
      columnsNew = intercalate "," $ map (escape . colName . snd) commonColumns
showAlterDb (DropTrigger _ name _ _) = Right [(False, triggerPriority, "DROP TRIGGER " ++ escape name)]
showAlterDb (AddTriggerOnDelete _ trigName _ tName body) = Right [(False, triggerPriority,
  "CREATE TRIGGER " ++ escape trigName ++ " DELETE ON " ++ escape tName ++ " BEGIN " ++ body ++ "END")]
showAlterDb (AddTriggerOnUpdate _ trigName _ tName fieldName body) = Right [(False, triggerPriority,
  "CREATE TRIGGER " ++ escape trigName ++ " UPDATE OF " ++ fieldName' ++ " ON " ++ escape tName ++ " BEGIN " ++ body ++ "END")] where
    fieldName' = maybe (error $ "showAlterDb: AddTriggerOnUpdate does not have fieldName for trigger " ++ trigName) escape fieldName
showAlterDb alt = error $ "showAlterDb: does not support " ++ show alt

showAlterTable :: String -> AlterTable -> Maybe (Bool, Int, String)
showAlterTable table (AddColumn col) = Just (False, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " ADD COLUMN "
  , showColumn col
  ])
showAlterTable table (AlterColumn col [UpdateValue s]) = Just (False, defaultPriority, concat
  [ "UPDATE "
  , escape table
  , " SET "
  , escape (colName col)
  , "="
  , s
  , " WHERE "
  , escape (colName col)
  , " IS NULL"
  ])
showAlterTable table (AddUnique (UniqueDef' uName UniqueIndex cols)) = Just (False, defaultPriority, concat
  [ "CREATE UNIQUE INDEX "
  , maybe (error $ "showAlterTable: index for table " ++ table ++ " does not have a name") escape uName
  , " ON "
  , escape table
  , "("
  , intercalate "," $ map escape cols
  , ")"
  ])
showAlterTable _ (DropIndex uName) = Just (False, defaultPriority, concat
  [ "DROP INDEX "
  , escape uName
  ])
showAlterTable _ _ = Nothing
