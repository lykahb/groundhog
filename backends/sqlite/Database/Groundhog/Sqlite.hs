{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies, RecordWildCards, Rank2Types #-}
module Database.Groundhog.Sqlite
    ( withSqlitePool
    , withSqliteConn
    , runSqlitePool
    , runSqliteConn
    , Sqlite
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

import qualified Database.Sqlite as S

import Control.Arrow ((***))
import Control.Monad (liftM, forM)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (groupBy, intercalate, isInfixOf, partition, sort)
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Data.Maybe (maybeToList)
import Data.Conduit.Pool
import qualified Data.Text as T

import GHC.Exts (inline)

-- typical operations for connection: OPEN, BEGIN, COMMIT, ROLLBACK, CLOSE
data Sqlite = Sqlite S.Database (IORef (Map.HashMap BS.ByteString S.Statement))

instance DbDescriptor Sqlite where
  type AutoKeyType Sqlite = Int64
  type QueryRaw Sqlite = Snippet Sqlite

instance SqlDb Sqlite where
  append a b = Expr $ operator 50 "||" a b

instance (MonadBaseControl IO m, MonadIO m) => PersistBackend (DbPersist Sqlite m) where
  {-# SPECIALIZE instance PersistBackend (DbPersist Sqlite IO) #-}
  type PhantomDb (DbPersist Sqlite m) = Sqlite
  insert v = insert' v
  insertBy u v = H.insertBy escapeS queryRawTyped u v
  insertByAll v = H.insertByAll escapeS queryRawTyped v
  replace k v = H.replace escapeS queryRawTyped executeRawCached' insertIntoConstructorTable k v
  select options = H.select escapeS queryRawTyped "LIMIT -1" renderCond' options -- select' options
  selectAll = H.selectAll escapeS queryRawTyped
  get k = inline $ H.get escapeS queryRawTyped k
  getBy k = H.getBy escapeS queryRawTyped k
  update upds cond = H.update escapeS executeRawCached' renderCond' upds cond
  delete cond = H.delete escapeS executeRawCached' renderCond' cond
  deleteByKey k = H.deleteByKey escapeS executeRawCached' k
  count cond = H.count escapeS queryRawTyped renderCond' cond
  countAll fakeV = H.countAll escapeS queryRawTyped fakeV
  project p options = H.project escapeS queryRawTyped "LIMIT -1" renderCond' p options
  migrate fakeV = migrate' fakeV

  executeRaw False query ps = executeRaw' (fromString query) ps
  executeRaw True query ps = executeRawCached' (fromString query) ps
  queryRaw False query ps f = queryRaw' (fromString query) ps f
  queryRaw True query ps f = queryRawCached' (fromString query) ps f

  insertList l = insertList' l
  getList k = getList' k

instance (MonadBaseControl IO m, MonadIO m) => SchemaAnalyzer (DbPersist Sqlite m) where
  listTables _ = queryRaw' "SELECT name FROM sqlite_master WHERE type='table'" [] (mapAllRows $ return . fst . fromPurePersistValues proxy)
  listTableTriggers _ name = queryRaw' "SELECT name FROM sqlite_master WHERE type='trigger' AND tbl_name=?" [toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
  analyzeTable = analyzeTable'
  analyzeTrigger _ name = do
    x <- queryRaw' "SELECT sql FROM sqlite_master WHERE type='trigger' AND name=?" [toPrimitivePersistValue proxy name] id
    case x of
      Nothing  -> return Nothing
      Just src -> return (fst $ fromPurePersistValues proxy src)
  analyzeFunction = error "analyzeFunction: is not supported by Sqlite"

--{-# SPECIALIZE INLINE H.get :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific)) => (Utf8 -> Utf8) -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper (DbPersist Sqlite m) -> DbPersist Sqlite m a) -> DbPersist Sqlite m a) -> Key v BackendSpecific -> DbPersist Sqlite m (Maybe v) #-}

--{-# SPECIALIZE withSqlitePool :: String -> Int -> (Pool Sqlite -> IO a) -> IO a #-}
withSqlitePool :: (MonadBaseControl IO m, MonadIO m)
               => String
               -> Int -- ^ number of connections to open
               -> (Pool Sqlite -> m a)
               -> m a
withSqlitePool s connCount f = liftIO (createPool (open' s) close' 1 20 connCount) >>= f

{-# SPECIALIZE withSqliteConn :: String -> (Sqlite -> IO a) -> IO a #-}
{-# INLINE withSqliteConn #-}
withSqliteConn :: (MonadBaseControl IO m, MonadIO m)
               => String
               -> (Sqlite -> m a)
               -> m a
withSqliteConn s = bracket (liftIO $ open' s) (liftIO . close')

{-# SPECIALIZE runSqlitePool :: DbPersist Sqlite IO a -> Pool Sqlite -> IO a #-}
runSqlitePool :: (MonadBaseControl IO m, MonadIO m) => DbPersist Sqlite m a -> Pool Sqlite -> m a
runSqlitePool f pconn = withResource pconn $ runSqliteConn f

{-# SPECIALIZE runSqliteConn :: DbPersist Sqlite IO a -> Sqlite -> IO a #-}
{-# INLINE runSqliteConn #-}
runSqliteConn :: (MonadBaseControl IO m, MonadIO m) => DbPersist Sqlite m a -> Sqlite -> m a
runSqliteConn f conn@(Sqlite c _) = do
  let runStmt query = S.prepare c query >>= \stmt -> S.step stmt >> S.finalize stmt
  liftIO $ runStmt "BEGIN"
  x <- onException (runDbPersist f conn) (liftIO $ runStmt "ROLLBACK")
  liftIO $ runStmt "COMMIT"
  return x

open' :: String -> IO Sqlite
open' s = do
  conn <- S.open s
  S.prepare conn "PRAGMA foreign_keys = ON" >>= \stmt -> S.step stmt >> S.finalize stmt
  cache <- newIORef Map.empty
  return $ Sqlite conn cache

close' :: Sqlite -> IO ()
close' (Sqlite conn smap) = do
  readIORef smap >>= mapM_ S.finalize . Map.elems
  S.close conn

migrate' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> Migration (DbPersist Sqlite m)
migrate' = migrateRecursively (migrateEntity migrationPack) (migrateList migrationPack)

migrationPack :: (MonadBaseControl IO m, MonadIO m) => GM.MigrationPack (DbPersist Sqlite m)
migrationPack = GM.MigrationPack
  compareTypes
  compareRefs
  compareUniqs
  migTriggerOnDelete
  migTriggerOnUpdate
  GM.defaultMigConstr
  escape
  "INTEGER PRIMARY KEY"
  "INTEGER"
  mainTableId
  defaultPriority
  addUniquesReferences
  showColumn
  showAlterDb

addUniquesReferences :: [UniqueDef'] -> [Reference] -> ([String], [AlterTable])
addUniquesReferences uniques refs = (map sqlUnique constraints ++ map sqlReference refs, map AddUnique indexes) where
  (constraints, indexes) = partition (\(UniqueDef' _ uType _) -> uType == UniqueConstraint) uniques

migTriggerOnDelete :: (MonadBaseControl IO m, MonadIO m) => Maybe String -> String -> [(String, String)] -> DbPersist Sqlite m (Bool, [AlterDB])
migTriggerOnDelete schema name deletes = do
  let addTrigger = AddTriggerOnDelete name name (concatMap snd deletes)
  x <- analyzeTrigger schema name
  return $ case x of
    Nothing | null deletes -> (False, [])
    Nothing -> (False, [addTrigger])
    Just sql -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
      then [DropTrigger name name]
      else if Right [(False, triggerPriority, sql)] == showAlterDb addTrigger
        then []
        -- this can happen when an ephemeral field was added or removed.
        else [DropTrigger name name, addTrigger])
        
-- | Schema name, table name and a list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: (MonadBaseControl IO m, MonadIO m) => Maybe String -> String -> String -> String -> DbPersist Sqlite m (Bool, [AlterDB])
migTriggerOnUpdate schema name fieldName del = do
  let trigName = name ++ delim : fieldName
  let addTrigger = AddTriggerOnUpdate trigName name fieldName del
  x <- analyzeTrigger schema trigName
  return $ case x of
    Nothing -> (False, [addTrigger])
    Just sql -> (True, if Right [(False, triggerPriority, sql)] == showAlterDb addTrigger
        then []
        else [DropTrigger trigName name, addTrigger])

analyzeTable' :: (MonadBaseControl IO m, MonadIO m) => Maybe String -> String -> DbPersist Sqlite m (Either [String] (Maybe TableInfo))
analyzeTable' _ tName = do
  let fromName = escapeS . fromString
  tableInfo <- queryRaw' ("pragma table_info(" <> fromName tName <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  case tableInfo of
    [] -> return $ Right Nothing
    xs -> do
      let rawColumns = filter (\(_ , (_, _, _, _, isPrimary)) -> isPrimary == 0) xs
      let mkColumn :: (Int, (String, String, Int, Maybe String, Int)) -> Column
          mkColumn (_, (name, typ, isNotNull, defaultValue, _)) = Column name (isNotNull == 0) (readSqlType typ) defaultValue
      let columns = map mkColumn rawColumns
      indexList <- queryRaw' ("pragma index_list(" <> fromName tName <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
      let uniqueNames = map (\(_ :: Int, name, _) -> name) $ filter (\(_, _, isUnique) -> isUnique) indexList
      uniques <- forM uniqueNames $ \name -> do
        uFields <- queryRaw' ("pragma index_info(" <> fromName name <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
        sql <- queryRaw' ("select sql from sqlite_master where type = 'index' and name = ?") [toPrimitivePersistValue proxy name] id
        let uType = if sql == Just [PersistNull] then UniqueConstraint else UniqueIndex
        return $ UniqueDef' name uType $ map (\(_, _, columnName) -> columnName) (uFields :: [(Int, Int, String)])
      foreignKeyList <- queryRaw' ("pragma foreign_key_list(" <> fromName tName <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
      (foreigns :: [(Maybe String, Reference)]) <- do
          let foreigns :: [[(Int, (Int, String, (String, Maybe String), (String, String, String)))]]
              foreigns = groupBy ((==) `on` fst) . sort $ foreignKeyList -- sort by foreign key number and column number inside key (first and second integers)
          forM foreigns $ \rows -> do 
            let (_, (_, foreignTable, _, _)) = head rows
            refs <- forM rows $ \(_, (_, _, (child, parent), _)) -> case parent of
              Nothing -> analyzePrimaryKey foreignTable >>= \x -> case x of
                Just primaryKeyName -> return (child, primaryKeyName)
                Nothing -> error $ "analyzeTable: cannot find primary key for table " ++ foreignTable ++ " which is referenced without specifying column names"
              Just columnName -> return (child, columnName)
            return (Nothing, (Nothing, foreignTable, refs))
      primaryKey <- analyzePrimaryKey tName
      return $ Right $ Just $ TableInfo primaryKey columns uniques foreigns

analyzePrimaryKey :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Sqlite m (Maybe String)
analyzePrimaryKey tName = do
  tableInfo <- queryRaw' ("pragma table_info(" <> escapeS (fromString tName) <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  let rawColumns :: [(Int, (String, String, Int, Maybe String, Int))]
      rawColumns = filter (\(_ , (_, _, _, _, isPrimary)) -> isPrimary == 1) tableInfo
  return $ case rawColumns of
    [(_, (name, _, _, _, _))] -> Just name
    -- if the list is empty, there is no primary key
    -- if the list has more than 1 element, the key is read by pragma index_list as a unique constraint in another place
    _ -> Nothing

getStatementCached :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> DbPersist Sqlite m S.Statement
getStatementCached sql = do
  Sqlite conn smap <- DbPersist ask
  liftIO $ do
    smap' <- readIORef smap
    let sql' = fromUtf8 sql
    case Map.lookup sql' smap' of
      Nothing -> do
        stmt <- S.prepare conn sql'
        writeIORef smap (Map.insert sql' stmt smap')
        return stmt
      Just stmt -> return stmt

getStatement :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> DbPersist Sqlite m S.Statement
getStatement sql = do
  Sqlite conn _ <- DbPersist ask
  liftIO $ S.prepare conn (fromUtf8 sql)

showSqlType :: DbType -> String
showSqlType DbString = "VARCHAR"
showSqlType DbInt32 = "INTEGER"
showSqlType DbInt64 = "INTEGER"
showSqlType DbReal = "REAL"
showSqlType DbBool = "BOOLEAN"
showSqlType DbDay = "DATE"
showSqlType DbTime = "TIME"
showSqlType DbDayTime = "TIMESTAMP"
showSqlType DbDayTimeZoned = "TIMESTAMP WITH TIME ZONE"
showSqlType DbBlob = "BLOB"
showSqlType (DbOther (OtherTypeDef f)) = f showSqlType
showSqlType (DbMaybe t) = showSqlType t
showSqlType (DbList _ _) = showSqlType DbInt64
showSqlType (DbEntity Nothing _) = showSqlType DbInt64
showSqlType t = error $ "showSqlType: DbType does not have corresponding database type: " ++ show t

readSqlType :: String -> DbType
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

dbTypeAffinity :: DbType -> Affinity
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
showColumn (Column name isNull typ _) = escape name ++ " " ++ showSqlType typ ++ rest where
  rest = if not isNull 
           then " NOT NULL"
           else ""

sqlReference :: Reference -> String
sqlReference (_, tname, columns) = "FOREIGN KEY(" ++ our ++ ") REFERENCES " ++ escape tname ++ "(" ++ foreign ++ ")" where
  (our, foreign) = f *** f $ unzip columns
  f = intercalate ", " . map escape

sqlUnique :: UniqueDef' -> String
sqlUnique (UniqueDef' name _ cols) = concat
    [ "CONSTRAINT "
    , escape name
    , " UNIQUE ("
    , intercalate "," $ map escape cols
    , ")"
    ]

{-# SPECIALIZE insert' :: PersistEntity v => v -> DbPersist Sqlite IO (AutoKey v) #-}
insert' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> DbPersist Sqlite m (AutoKey v)
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

-- TODO: In Sqlite we can insert null to the id column. If so, id will be generated automatically. Check performance change from this.
insertIntoConstructorTable :: Bool -> Utf8 -> ConstructorDef -> [PersistValue] -> RenderS db r
insertIntoConstructorTable withId tName c vals = RenderS query vals' where
  query = "INSERT INTO " <> tName <> "(" <> fieldNames <> ")VALUES(" <> placeholders <> ")"
  fields = case constrAutoKeyName c of
    Just idName | withId -> (idName, dbType (0 :: Int64)):constrParams c
    _                    -> constrParams c
  fieldNames   = renderFields escapeS fields
  RenderS placeholders vals' = commasJoin $ map renderPersistValue vals

insertList' :: forall m a.(MonadBaseControl IO m, MonadIO m, PersistField a) => [a] -> DbPersist Sqlite m Int64
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
  
getList' :: forall m a.(MonadBaseControl IO m, MonadIO m, PersistField a) => Int64 -> DbPersist Sqlite m [a]
getList' k = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  let valuesName = mainName <> delim' <> "values"
  let value = ("value", dbType (undefined :: a))
  let query = "SELECT " <> renderFields escapeS [value] <> " FROM " <> escapeS valuesName <> " WHERE id=? ORDER BY ord"
  queryRawTyped query (getDbTypes (dbType (undefined :: a)) []) [toPrimitivePersistValue proxy k] $ mapAllRows (liftM fst . fromPersistValues)
    
{-# SPECIALIZE getLastInsertRowId :: DbPersist Sqlite IO PersistValue #-}
getLastInsertRowId :: (MonadBaseControl IO m, MonadIO m) => DbPersist Sqlite m PersistValue
getLastInsertRowId = do
  stmt <- getStatementCached "SELECT last_insert_rowid()"
  liftIO $ flip finally (liftIO $ S.reset stmt) $ do
    S.step stmt
    x <- S.column stmt 0
    return $ pFromSql x

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

executeRaw' :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> [PersistValue] -> DbPersist Sqlite m ()
executeRaw' query vals = do
  stmt <- getStatement query
  liftIO $ flip finally (S.finalize stmt) $ do
    bind stmt vals
    S.Done <- S.step stmt
    return ()

{-# SPECIALIZE executeRawCached' :: Utf8 -> [PersistValue] -> DbPersist Sqlite IO () #-}
executeRawCached' :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> [PersistValue] -> DbPersist Sqlite m ()
executeRawCached' query vals = do
  stmt <- getStatementCached query
  liftIO $ flip finally (S.reset stmt) $ do
    bind stmt vals
    S.Done <- S.step stmt
    return ()

queryRaw' :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> [PersistValue] -> (RowPopper (DbPersist Sqlite m) -> DbPersist Sqlite m a) -> DbPersist Sqlite m a
queryRaw' query vals f = do
  stmt <- getStatement query
  flip finally (liftIO $ S.finalize stmt) $ do
    liftIO $ bind stmt vals
    f $ liftIO $ do
      x <- S.step stmt
      case x of
        S.Done -> return Nothing
        S.Row  -> liftM (Just . map pFromSql) $ S.columns stmt

queryRawCached' :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> [PersistValue] -> (RowPopper (DbPersist Sqlite m) -> DbPersist Sqlite m a) -> DbPersist Sqlite m a
queryRawCached' query vals f = do
  stmt <- getStatementCached query
  flip finally (liftIO $ S.reset stmt) $ do
    liftIO $ bind stmt vals
    f $ liftIO $ do
      x <- S.step stmt
      case x of
        S.Done -> return Nothing
        S.Row  -> fmap (Just . map pFromSql) $ S.columns stmt

queryRawTyped :: (MonadBaseControl IO m, MonadIO m) => Utf8 -> [DbType] -> [PersistValue] -> (RowPopper (DbPersist Sqlite m) -> DbPersist Sqlite m a) -> DbPersist Sqlite m a
queryRawTyped query types vals f = do
  stmt <- getStatementCached query
  let types' = map typeToSqlite types
  flip finally (liftIO $ S.reset stmt) $ do
    liftIO $ bind stmt vals
    f $ liftIO $ do
      x <- S.step stmt
      case x of
        S.Done -> return Nothing
        S.Row  -> fmap (Just . map pFromSql) $ S.unsafeColumns stmt types'

typeToSqlite :: DbType -> Maybe S.ColumnType
typeToSqlite DbString = Just S.TextColumn
typeToSqlite DbInt32 = Just S.IntegerColumn
typeToSqlite DbInt64 = Just S.IntegerColumn
typeToSqlite DbReal = Just S.FloatColumn
typeToSqlite DbBool = Just S.IntegerColumn
typeToSqlite DbDay = Nothing
typeToSqlite DbTime = Nothing
typeToSqlite DbDayTime = Nothing
typeToSqlite DbDayTimeZoned = Nothing
typeToSqlite DbBlob = Just S.BlobColumn
typeToSqlite (DbOther _) = Nothing
typeToSqlite (DbMaybe _) = Nothing
typeToSqlite (DbList _ _) = Just S.IntegerColumn
typeToSqlite (DbEntity Nothing _) = Just S.IntegerColumn
typeToSqlite t = error $ "typeToSqlite: DbType does not have corresponding database type: " ++ show t

getDbTypes :: DbType -> [DbType] -> [DbType]
getDbTypes typ acc = case typ of
  DbEmbedded (EmbeddedDef _ ts) -> foldr (getDbTypes . snd) acc ts
  DbEntity (Just (EmbeddedDef _ ts, _)) _ -> foldr (getDbTypes . snd) acc ts
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

renderCond' :: Cond Sqlite r -> Maybe (RenderS Sqlite r)
renderCond' = renderCond escapeS renderEquals renderNotEquals where
  renderEquals a b = a <> " IS " <> b
  renderNotEquals a b = a <> " IS NOT " <> b

defaultPriority :: Int
defaultPriority = 0

triggerPriority :: Int
triggerPriority = 1

proxy :: Proxy Sqlite
proxy = error "Proxy Sqlite"

delim' :: Utf8
delim' = fromChar delim

toEntityPersistValues' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Sqlite m [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

compareUniqs :: UniqueDef' -> UniqueDef' -> Bool
compareUniqs (UniqueDef' _ type1 cols1) (UniqueDef' _ type2 cols2) = haveSameElems (==) cols1 cols2 && type1 == type2

compareRefs :: (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
compareRefs (_, (_, tbl1, pairs1)) (_, (_, tbl2, pairs2)) = unescape tbl1 == unescape tbl2 && haveSameElems (==) pairs1 pairs2 where
  unescape name = if head name == '"' && last name == '"' then tail $ init name else name

compareTypes :: DbType -> DbType -> Bool
compareTypes type1 type2 = dbTypeAffinity type1 == dbTypeAffinity type2
--compareTypes type1 type2 = showSqlType type1 == showSqlType type2

mainTableId :: String
mainTableId = "id"

showAlterDb :: AlterDB -> SingleMigration
showAlterDb (AddTable s) = Right [(False, defaultPriority, s)]
showAlterDb (AlterTable _ table createTable (TableInfo oldId oldCols _ _) (TableInfo newId newCols _ _) alts) = case mapM (showAlterTable table) alts of
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
      columnsTmp = intercalate "," $ map escape $ maybeToList (newId >> oldId) ++ map (colName . snd) commonColumns
      columnsNew = intercalate "," $ map escape $ maybeToList (oldId >> newId) ++ map (colName . snd) commonColumns
showAlterDb (DropTrigger name _) = Right [(False, triggerPriority, "DROP TRIGGER " ++ escape name)]
showAlterDb (AddTriggerOnDelete trigName tName body) = Right [(False, triggerPriority,
  "CREATE TRIGGER " ++ escape trigName ++ " DELETE ON " ++ escape tName ++ " BEGIN " ++ body ++ "END")]
showAlterDb (AddTriggerOnUpdate trigName tName fieldName body) = Right [(False, triggerPriority,
  "CREATE TRIGGER " ++ escape trigName ++ " UPDATE OF " ++ escape fieldName ++ " ON " ++ escape tName ++ " BEGIN " ++ body ++ "END")]
showAlterDb alt = error $ "showAlterDb: does not support " ++ show alt

showAlterTable :: String -> AlterTable -> Maybe (Bool, Int, String)
showAlterTable table (AlterColumn alt) = showAlterColumn table alt
showAlterTable table (AddUnique (UniqueDef' uName UniqueIndex cols)) = Just (False, defaultPriority, concat
  [ "CREATE UNIQUE INDEX "
  , escape uName
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

showAlterColumn :: String -> AlterColumn' -> Maybe (Bool, Int, String)
showAlterColumn table (_, Add col) = Just (False, defaultPriority, concat
    [ "ALTER TABLE "
    , escape table
    , " ADD COLUMN "
    , showColumn col
    ])
showAlterColumn table (n, UpdateValue s) = Just (False, defaultPriority, concat
    [ "UPDATE "
    , escape table
    , " SET "
    , escape n
    , "="
    , s
    , " WHERE "
    , escape n
    , " IS NULL"
    ])
showAlterColumn _ _ = Nothing
