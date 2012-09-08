{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Database.Groundhog.Sqlite
    ( withSqlitePool
    , withSqliteConn
    , runSqlitePool
    , runSqliteConn
    , Sqlite
    , module Database.Groundhog
    ) where

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Migration
import Database.Groundhog.Generic.Sql.Utf8

import qualified Database.Sqlite as S

import Control.Arrow ((&&&), (***))
import Control.Monad (liftM, forM, (>=>))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.Function (on)
import Data.Int (Int32, Int64)
import Data.List (group, groupBy, intercalate, isInfixOf, sort)
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe, maybeToList)
import Data.Monoid
import Data.Conduit.Pool

-- typical operations for connection: OPEN, BEGIN, COMMIT, ROLLBACK, CLOSE
data Sqlite = Sqlite S.Database (IORef (Map.HashMap BS.ByteString S.Statement))

instance DbDescriptor Sqlite where
  type AutoKeyType Sqlite = Int32

instance (MonadBaseControl IO m, MonadIO m) => PersistBackend (DbPersist Sqlite m) where
  {-# SPECIALIZE instance PersistBackend (DbPersist Sqlite IO) #-}
  type PhantomDb (DbPersist Sqlite m) = Sqlite
  insert v = insert' v
  insertBy u v = insertBy' u v
  insertByAll v = insertByAll' v
  replace k v = replace' k v
  select options = select' options
  selectAll = selectAll'
  get k = get' k
  getBy k = getBy' k
  update upds cond = update' upds cond
  delete cond = delete' cond
  deleteByKey k = deleteByKey' k
  count cond = count' cond
  countAll fakeV = countAll' fakeV
  project p options = project' p options
  migrate fakeV = migrate' fakeV

  executeRaw False query ps = executeRaw' (fromString query) ps
  executeRaw True query ps = executeRawCached' (fromString query) ps
  queryRaw False query ps f = queryRaw' (fromString query) ps f
  queryRaw True query ps f = queryRawCached' (fromString query) ps f

  insertList l = insertList' l
  getList k = getList' k

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

{- ********************RULES******************** --
For type with a single constructor, a single table is created.
TABLE Entity(id, [fields])
If constructor has no fields, then ????

For type with a multiple constructors, the main table is created.
TABLE(id, discriminator)
where discriminator is defined by constructor.
Each constructor has its table, where id is the same as in 
TABLE EntityConstructor2(id, [fields])

In Java Hibernate each class member of list type is stored in a separate table
TABLE Student$Phones(studentId, phone)
Here we can use triggers to automatically remove list after Student removal.
However, toPersistValue :: a -> DbPersist conn m () becomes impossible because we must know container id

We can either follow this scheme or store same type lists from different types in one table
TABLE List$Int(id, value)

-- ********************************************* --}
migrate' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> Migration (DbPersist Sqlite m)
migrate' = migrateRecursively migE migL where
  migE e = do
    let name = entityName e
    let constrs = constructors e
    let mainTableQuery = "CREATE TABLE " ++ escape name ++ " (" ++ mainTableId ++ " INTEGER PRIMARY KEY, discr INTEGER NOT NULL)"
    let mainTableColumns = [Column "discr" False DbInt32 Nothing]
    
    if isSimple constrs
      then do
        x <- checkTable name
        -- check whether the table was created for multiple constructors before
        case x of
          Just (Right (_, columns, _, _)) | haveSameElems compareColumns columns mainTableColumns -> do
            return $ Left ["Datatype with multiple constructors was truncated to one constructor. Manual migration required. Datatype: " ++ name]
          Just (Left errs) -> return (Left errs)
          _ -> liftM snd $ migConstr True name $ head constrs
      else do
        maincolumns <- checkTable name
        let constrTable c = name ++ [delim] ++ constrName c
        res <- mapM (\c -> migConstr False name c) constrs
        case maincolumns of
          Nothing -> do
            -- no constructor tables can exist if there is no main data table
            let orphans = filter (fst . fst) $ zip res constrs
            return $ if null orphans
              then mergeMigrations $ Right [(False, defaultPriority, mainTableQuery)]:map snd res
              else Left $ map (\(_, c) -> "Orphan constructor table found: " ++ constrTable c) orphans
          Just (Right (Just _, columns, [], [])) -> do
            if haveSameElems compareColumns columns mainTableColumns
              then do
                -- the datatype had also many constructors before
                -- check whether any new constructors appeared and increment older discriminators, which were shifted by newer constructors inserted not in the end
                let updateDiscriminators = Right . go 0 . map (head &&& length) . group . map fst $ res where
                    go acc ((False, n):(True, n2):xs) = (False, defaultPriority, "UPDATE " ++ escape name ++ " SET discr = discr + " ++ show n ++ " WHERE discr >= " ++ show acc) : go (acc + n + n2) xs
                    go acc ((True, n):xs) = go (acc + n) xs
                    go _ _ = []
                return $ mergeMigrations $ updateDiscriminators: (map snd res)
              else do
                return $ Left ["Migration from one constructor to many will be implemented soon. Datatype: " ++ name]
          Just (Right structure) -> do
            return $ Left ["Unexpected structure of main table for Datatype: " ++ name ++ ". Table info: " ++ show structure]
          Just (Left errs) -> return (Left errs)
  migL (DbList mainName t) = do
    let valuesName = mainName ++ delim : "values"
    let (valueCols, valueRefs) = mkColumns id "value" t
    let mainQuery = "CREATE TABLE " ++ escape mainName ++ " (id INTEGER PRIMARY KEY)"
    let items = ("id INTEGER NOT NULL REFERENCES " ++ escape mainName ++ " ON DELETE CASCADE"):"ord INTEGER NOT NULL" : map showColumn valueCols ++ map sqlReference valueRefs
    let valuesQuery = "CREATE TABLE " ++ escape valuesName ++ " (" ++ intercalate ", " items ++ ")"
    let expectedMainStructure = (Just "id", [], [], [])
    let expectedValuesStructure = (Nothing, Column "id" False (dbType (0 :: Int32)) Nothing : Column "ord" False (dbType (0 :: Int32)) Nothing : valueCols, [], map (\x -> (Nothing, x)) $ (mainName, [("id", "id")]) : valueRefs)
    mainStructure <- checkTable mainName
    valuesStructure <- checkTable valuesName
    let triggerMain = []
    (_, triggerValues) <- migTriggerOnDelete valuesName $ mkDeletes valueCols
    return $ case (mainStructure, valuesStructure) of
      (Nothing, Nothing) -> let addReferences = []
        in mergeMigrations $ map showAlterDb $ [AddTable mainQuery, AddTable valuesQuery] ++ addReferences ++ triggerMain ++ triggerValues
      (Just (Right mainStructure'), Just (Right valuesStructure')) -> let
        f name a@(id1, cols1, uniqs1, refs1) b@(id2, cols2, uniqs2, refs2) = if id1 == id2 && haveSameElems compareColumns cols1 cols2 && haveSameElems compareUniqs uniqs1 uniqs2 && haveSameElems compareRefs refs1 refs2
          then []
          else ["List table " ++ name ++ " error. Expected: " ++ show b ++ ". Found: " ++ show a]
        errors = f mainName mainStructure' expectedMainStructure ++ f valuesName valuesStructure' expectedValuesStructure
        in if null errors then Right [] else Left errors
      (Just (Left errs1), Just (Left errs2)) -> Left $ errs1 ++ errs2
      (Just (Left errs), Just _) -> Left errs
      (Just _, Just (Left errs)) -> Left errs
      (_, Nothing) -> Left ["Found orphan main list table " ++ mainName]
      (Nothing, _) -> Left ["Found orphan list values table " ++ valuesName]
  migL t = fail $ "migrate: expected DbList, got " ++ show t

migConstr :: (MonadBaseControl IO m, MonadIO m) => Bool -> String -> ConstructorDef -> DbPersist Sqlite m (Bool, SingleMigration)
migConstr simple name constr = do
  let cName = if simple then name else name ++ [delim] ++ constrName constr
  let mkColumns' xs = concat *** concat $ unzip $ map (uncurry $ mkColumns id) xs
  let (columns, refs) = mkColumns' $ constrParams constr
  tableStructure <- checkTable cName
  let dels = mkDeletes columns
  (triggerExisted, delTrigger) <- migTriggerOnDelete cName dels
  updTriggers <- liftM concat $ mapM (liftM snd . uncurry (migTriggerOnUpdate cName)) dels
  
  let mainTableName = if simple then Nothing else Just name
      refs' = maybeToList (fmap (\x -> (x, [(fromJust $ constrAutoKeyName constr, mainTableId)])) mainTableName) ++ refs

      mainRef = maybe "" (\x -> " REFERENCES " ++ escape x ++ " ON DELETE CASCADE ") mainTableName
      autoKey = fmap (\x -> escape x ++ " INTEGER PRIMARY KEY" ++ mainRef) $ constrAutoKeyName constr
  
      uniques = map (\(UniqueDef uName cols) -> UniqueDef' uName (map colName $ fst $ mkColumns' cols)) $ constrUniques constr
      -- refs instead of refs' because the reference to the main table id is hardcoded in mainRef
      items = maybeToList autoKey ++ map showColumn columns ++ map sqlUnique uniques ++ map sqlReference refs
      addTable = "CREATE TABLE " ++ escape cName ++ " (" ++ intercalate ", " items ++ ")"

      expectedTableStructure = (constrAutoKeyName constr, columns, uniques, map (\r -> (Nothing, r)) refs')
      (migErrs, constrExisted, mig) = case tableStructure of
        Nothing  -> ([], False, [AddTable addTable])
        Just (Right oldTableStructure) -> let
          alters = getAlters oldTableStructure expectedTableStructure
          in ([], True, [AlterTable cName addTable oldTableStructure expectedTableStructure alters])
        Just (Left x) -> (x, True, [])
      -- this can happen when an ephemeral field was added. Consider doing something else except throwing an error
      errs = if constrExisted == triggerExisted || (constrExisted && null dels)
        then migErrs
        else ["Both trigger and constructor table must exist: " ++ cName] ++ migErrs
  return $ (constrExisted, if null errs
    then mergeMigrations $ map showAlterDb $ mig ++ delTrigger ++ updTriggers
    else Left errs)

-- it handles only delete operations. So far when list replace is not allowed, it is ok
migTriggerOnDelete :: (MonadBaseControl IO m, MonadIO m) => String -> [(String, String)] -> DbPersist Sqlite m (Bool, [AlterDB Affinity])
migTriggerOnDelete name deletes = do
  let addTrigger = AddTriggerOnDelete name name (concatMap snd deletes)
  x <- checkTrigger name
  return $ case x of
    Nothing | null deletes -> (False, [])
    Nothing -> (False, [addTrigger])
    Just sql -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
      then [DropTrigger name name]
      else if Right [(False, triggerPriority, sql)] == showAlterDb addTrigger
        then []
        -- this can happen when an ephemeral field was added or removed.
        else [DropTrigger name name, addTrigger])
        
-- | Table name and a  list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: (MonadBaseControl IO m, MonadIO m) => String -> String -> String -> DbPersist Sqlite m (Bool, [AlterDB Affinity])
migTriggerOnUpdate name fieldName del = do
  let trigName = name ++ delim : fieldName
  let addTrigger = AddTriggerOnUpdate trigName name fieldName del
  x <- checkTrigger trigName
  return $ case x of
    Nothing -> (False, [addTrigger])
    Just sql -> (True, if Right [(False, triggerPriority, sql)] == showAlterDb addTrigger
        then []
        else [DropTrigger trigName name, addTrigger])
  
-- on delete removes all ephemeral data
-- returns column name and delete statement for the referenced table
mkDeletes :: [Column DbType] -> [(String, String)]
mkDeletes columns = mapMaybe delField columns where
  delField (Column name _ t _) = fmap delStatement $ ephemeralName t where
    delStatement ref = (name, "DELETE FROM " ++ escape ref ++ " WHERE id=old." ++ escape name ++ ";")
  ephemeralName (DbMaybe x) = ephemeralName x
  ephemeralName (DbList name _) = Just name
  ephemeralName _ = Nothing

checkTrigger :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Sqlite m (Maybe String)
checkTrigger = checkSqliteMaster "trigger"

checkSqliteMaster :: (MonadBaseControl IO m, MonadIO m) => String -> String -> DbPersist Sqlite m (Maybe String)
checkSqliteMaster vtype name = do
  let query = "SELECT sql FROM sqlite_master WHERE type = ? AND name = ?"
  x <- queryRawTyped query [DbString] [toPrimitivePersistValue proxy vtype, toPrimitivePersistValue proxy name] id
  let throwErr = error . ("Unexpected result from sqlite_master: " ++)
  case x of
    Nothing -> return Nothing
    Just [hsql] -> case hsql of
      PersistString sql -> return $ Just sql
      err               -> throwErr $ "column sql is not string: " ++ show err
    Just xs -> throwErr $ "requested 1 column, returned " ++ show xs

checkTable :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Sqlite m (Maybe (Either [String] (TableInfo Affinity)))
checkTable tableName = do
  let fromName = escapeS . fromString
  tableInfo <- queryRaw' ("pragma table_info(" <> fromName tableName <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  case tableInfo of
    [] -> return Nothing
    xs -> liftM Just $ do
      let rawColumns = filter (\(_ , (_, _, _, _, isPrimary)) -> isPrimary == 0) xs
      let mkColumn :: (Int, (String, String, Int, Maybe String, Int)) -> Column Affinity
          mkColumn (_, (name, typ, isNotNull, defaultValue, _)) = Column name (isNotNull == 0) (readSqlTypeAffinity typ) defaultValue
      let columns = map mkColumn rawColumns
      indexList <- queryRaw' ("pragma index_list(" <> fromName tableName <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
      let uniqueNames = map (\(_ :: Int, name, _) -> name) $ filter (\(_, _, isUnique) -> isUnique) indexList
      uniques <- forM uniqueNames $ \name -> do
        let mkUnique :: [(Int, Int, String)] -> UniqueDef'
            mkUnique us = UniqueDef'
              ""
              (map (\(_, _, columnName) -> columnName) us)
        queryRaw' ("pragma index_info(" <> fromName name <> ")") [] $ liftM mkUnique . mapAllRows (return . fst . fromPurePersistValues proxy)
      foreignKeyList <- queryRaw' ("pragma foreign_key_list(" <> fromName tableName <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
      (foreigns :: [(Maybe String, Reference)]) <- do
          let foreigns :: [[(Int, (Int, String, (String, Maybe String), (String, String, String)))]]
              foreigns = groupBy ((==) `on` fst) . sort $ foreignKeyList -- sort by foreign key number and column number inside key (first and second integers)
          --mkForeignKey :: [(Int, (Int, String, (String, Maybe String), (String, String, String)))] -> (Maybe String, Reference)
          forM foreigns $ \rows -> do 
            let (_, (_, foreignTable, _, _)) = head rows
            refs <- forM rows $ \(_, (_, _, (child, parent), _)) -> case parent of
              Nothing -> checkPrimaryKey foreignTable >>= \x -> case x of
                Left (Just primaryKeyName) -> return (child, primaryKeyName)
                _ -> error $ "checkTable: cannot find primary key for table " ++ foreignTable ++ " which is referenced without specifying column names"
              Just columnName -> return (child, columnName)
            return (Nothing, (foreignTable, refs))
      primaryKeyResult <- checkPrimaryKey tableName
      let (primaryKey, uniques') = case primaryKeyResult of
            Left primaryKeyName -> (primaryKeyName, uniques)
            Right u -> (Nothing, u:uniques)
      return $ Right (primaryKey, columns, uniques', foreigns)

checkPrimaryKey :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Sqlite m (Either (Maybe String) UniqueDef')
checkPrimaryKey tableName = do
  tableInfo <- queryRaw' ("pragma table_info(" <> escapeS (fromString tableName) <> ")") [] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  let rawColumns :: [(Int, (String, String, Int, Maybe String, Int))]
      rawColumns = filter (\(_ , (_, _, _, _, isPrimary)) -> isPrimary == 1) tableInfo
  return $ case rawColumns of
    [] -> Left Nothing
    [(_, (name, _, _, _, _))] -> Left (Just name)
    us -> Right $ UniqueDef' "" (map (\(_, (name, _, _, _, _)) -> name) us)
    
-- from database, from datatype
getAlters :: TableInfo Affinity
          -> TableInfo DbType
          -> [AlterTable]
getAlters (oldId, oldColumns, oldUniques, oldRefs) (newId, newColumns, newUniques, newRefs) = map AlterColumn colAlters ++ tableAlters
  where
    (oldOnlyColumns, newOnlyColumns, matchedColumns) = matchElements compareColumns oldColumns newColumns
    (oldOnlyUniques, newOnlyUniques, matchedUniques) = matchElements compareUniqs oldUniques newUniques
    primaryKeyAlters = case (oldId, newId) of
      (Nothing, Just newName) -> [(newName, AddPrimaryKey)]
      (Just oldName, Nothing) -> [(oldName, Drop)]
      (Just oldName, Just newName) | oldName /= newName -> error $ "getAlters: cannot rename primary key (old " ++ oldName ++ ", new " ++ newName ++ ")"
      _ -> []
    (oldOnlyRefs, newOnlyRefs, _) = matchElements compareRefs oldRefs newRefs

    colAlters = map (\x -> (colName x, Drop)) oldOnlyColumns ++ map (\x -> (colName x, Add x)) newOnlyColumns ++ concatMap migrateColumn matchedColumns ++ primaryKeyAlters
    tableAlters = 
         map (\(UniqueDef' name _) -> DropConstraint name) oldOnlyUniques
      ++ map (\(UniqueDef' name cols) -> AddUniqueConstraint name cols) newOnlyUniques
      ++ concatMap migrateUniq matchedUniques
      ++ map (DropReference . fromMaybe (error "getAlters: old reference does not have name") . fst) oldOnlyRefs
      ++ map (AddReference . snd) newOnlyRefs

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
showSqlType DbDayTimeZoned = "TIMESTAMP"
showSqlType DbBlob = "BLOB"
showSqlType (DbMaybe t) = showSqlType t
showSqlType (DbList _ _) = "INTEGER"
showSqlType (DbEntity Nothing _) = "INTEGER"
showSqlType t = error $ "showSqlType: DbType does not have corresponding database type: " ++ show t

data Affinity = TEXT | NUMERIC | INTEGER | REAL | NONE deriving (Eq, Show)

dbTypeAffinity :: DbType -> Affinity
dbTypeAffinity DbString = TEXT
dbTypeAffinity DbInt32 = INTEGER
dbTypeAffinity DbInt64 = INTEGER
dbTypeAffinity DbReal = REAL
dbTypeAffinity DbBool = NUMERIC
dbTypeAffinity DbDay = NUMERIC
dbTypeAffinity DbTime = NUMERIC
dbTypeAffinity DbDayTime = NUMERIC
dbTypeAffinity DbDayTimeZoned = NUMERIC
dbTypeAffinity DbBlob = NONE
dbTypeAffinity (DbMaybe t) = dbTypeAffinity t
dbTypeAffinity (DbList _ _) = INTEGER
dbTypeAffinity (DbEntity Nothing _) = INTEGER
dbTypeAffinity t = error $ "showSqlType: DbType does not have corresponding database type: " ++ show t

readSqlTypeAffinity :: String -> Affinity
readSqlTypeAffinity typ = affinity where
  contains = any (`isInfixOf` map toUpper typ)
  affinity = case () of
    _ | contains ["INT"] -> INTEGER
    _ | contains ["CHAR", "CLOB", "TEXT"]  -> TEXT
    _ | contains ["BLOB"] || null typ -> NONE
    _ | contains ["REAL", "FLOA", "DOUB"]  -> REAL
    _ -> NUMERIC

showColumn :: Column DbType -> String
showColumn (Column name isNull typ _) = escape name ++ " " ++ showSqlType typ ++ rest where
  rest = if not isNull 
           then " NOT NULL"
           else ""

sqlReference :: Reference -> String
sqlReference (tname, columns) = "FOREIGN KEY(" ++ our ++ ") REFERENCES " ++ escape tname ++ "(" ++ foreign ++ ")" where
  (our, foreign) = f *** f $ unzip columns
  f = intercalate ", " . map escape

sqlUnique :: UniqueDef' -> String
sqlUnique (UniqueDef' name cols) = concat
    [ "CONSTRAINT "
    , escape name
    , " UNIQUE ("
    , intercalate "," $ map escape cols
    , ")"
    ]

{-# SPECIALIZE insert' :: PersistEntity v => v -> DbPersist Sqlite IO (AutoKey v) #-}
{-# INLINE insert' #-}
insert' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> DbPersist Sqlite m (AutoKey v)
insert' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues' v
  let e = entityDef v
  let name = persistName v
  let constructorNum = fromPrimitivePersistValue proxy (head vals)

  liftM fst $ if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let query = insertIntoConstructorTable False name constr
      executeRawCached' query (tail vals)
      case constrAutoKeyName constr of
        Nothing -> pureFromPersistValue []
        Just _  -> getLastInsertRowId >>= \rowid -> pureFromPersistValue [rowid]
    else do
      let constr = constructors e !! constructorNum
      let cName = name ++ [delim] ++ constrName constr
      let query = "INSERT INTO " <> escapeS (fromString name) <> "(discr)VALUES(?)"
      executeRawCached' query $ take 1 vals
      rowid <- getLastInsertRowId
      let cQuery = insertIntoConstructorTable True cName constr
      executeRawCached' cQuery $ rowid:(tail vals)
      pureFromPersistValue [rowid]

insertBy' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
          => u (UniqueMarker v) -> v -> DbPersist Sqlite m (Either (AutoKey v) (AutoKey v))
insertBy' u v = do
  let e = entityDef v
  let name = persistName v
  uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
  let fields = foldr (renderChain escapeS) [] $ projectionFieldChains u []
  let cond = intercalateS " AND " $ map (<> "=?") fields
  
  let ifAbsent tname constr = do
      let query = "SELECT " <> maybe "1" id (constrId constr) <> " FROM " <> escapeS (fromString tname) <> " WHERE " <> cond
      x <- queryRawTyped query [DbInt32] (uniques []) id
      case x of
        Nothing  -> liftM Right $ insert v
        Just [k] -> return $ Left $ fst $ fromPurePersistValues proxy [k]
        Just xs  -> fail $ "unexpected query result: " ++ show xs
  let constr = head $ constructors e
  ifAbsent name constr

-- TODO: In Sqlite we can insert null to the id column. If so, id will be generated automatically. Check performance change from this.
insertIntoConstructorTable :: Bool -> String -> ConstructorDef -> Utf8
insertIntoConstructorTable withId tName c = "INSERT INTO " <> escapeS (fromString tName) <> "(" <> fieldNames <> ")VALUES(" <> placeholders <> ")" where
  fields = case constrAutoKeyName c of
    Just idName | withId -> (idName, dbType (0 :: Int32)):constrParams c
    _                    -> constrParams c
  fieldNames   = renderFields escapeS fields
  placeholders = renderFields (const $ fromChar '?') fields

{-# SPECIALIZE insertByAll' :: PersistEntity v => v -> DbPersist Sqlite IO (Either (AutoKey v) (AutoKey v)) #-}
insertByAll' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Sqlite m (Either (AutoKey v) (AutoKey v))
insertByAll' v = do
  let e = entityDef v
  let name = persistName v

  let (constructorNum, uniques) = getUniques proxy v
  let uniqueDefs = constrUniques $ constructors e !! constructorNum
  let cond = fromString $ intercalate " OR " $ map (intercalate " AND " . map (\(fname, _) -> escape fname ++ "=?")) $ map (\(UniqueDef _ fields) -> fields) uniqueDefs

  let ifAbsent tname constr = do
      let query = "SELECT " <> maybe "1" id (constrId constr) <> " FROM " <> escapeS (fromString tname) <> " WHERE " <> cond
      x <- queryRawTyped query [DbInt32] (concatMap snd uniques) id
      case x of
        Nothing  -> liftM Right $ insert v
        Just [k] -> return $ Left $ fst $ fromPurePersistValues proxy [k]
        Just xs  -> fail $ "unexpected query result: " ++ show xs
  if null uniques
    then liftM Right $ insert v
    else if isSimple (constructors e)
      then do
        let constr = head $ constructors e
        ifAbsent name constr
      else do
        let constr = constructors e !! constructorNum
        let cName = name ++ [delim] ++ constrName constr
        ifAbsent cName constr

replace' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
         => Key v BackendSpecific -> v -> DbPersist Sqlite m ()
replace' k v = do
  vals <- toEntityPersistValues' v
  let e = entityDef v
  let name = persistName v
  let constructorNum = fromPrimitivePersistValue proxy (head vals)
  let constr = constructors e !! constructorNum

  let upds = renderFields (\f -> escapeS f <> "=?") $ constrParams constr
  let mkQuery tname = "UPDATE " <> escapeS (fromString tname) <> " SET " <> upds <> " WHERE " <> fromString (fromJust $ constrAutoKeyName constr) <> "=?"

  if isSimple (constructors e)
    then executeRawCached' (mkQuery name) (tail vals ++ [toPrimitivePersistValue proxy k])
    else do
      let query = "SELECT discr FROM " <> escapeS (fromString name) <> " WHERE id=?"
      x <- queryRawTyped query [DbInt32] [toPrimitivePersistValue proxy k] (id >=> return . fmap (fromPrimitivePersistValue proxy . head))
      case x of
        Just discr -> do
          let cName = name ++ [delim] ++ constrName constr

          if discr == constructorNum
            then executeRawCached' (mkQuery cName) (tail vals ++ [toPrimitivePersistValue proxy k])
            else do
              let insQuery = insertIntoConstructorTable True cName constr
              executeRawCached' insQuery (toPrimitivePersistValue proxy k:tail vals)

              let oldCName = fromString $ name ++ [delim] ++ constrName (constructors e !! discr)
              let delQuery = "DELETE FROM " <> escapeS oldCName <> " WHERE " <> fromJust (constrId constr) <> "=?"
              executeRawCached' delQuery [toPrimitivePersistValue proxy k]

              let updateDiscrQuery = "UPDATE " <> escapeS (fromString name) <> " SET discr=? WHERE id=?"
              executeRawCached' updateDiscrQuery [head vals, toPrimitivePersistValue proxy k]
        Nothing -> return ()

select' :: forall m v c opts . (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c, HasSelectOptions opts v c)
        => opts -> DbPersist Sqlite m [v]
select' options = start where
  SelectOptions cond limit offset ords = getSelectOptions options
  start = if isSimple (constructors e)
    then doSelectQuery (mkQuery name) 0
    else let
      cName = name ++ [delim] ++ constrName constr
      in doSelectQuery (mkQuery cName) $ constrNum constr

  e = entityDef (undefined :: v)
  orders = renderOrders escapeS ords
  name = persistName (undefined :: v)
  (lim, limps) = case (limit, offset) of
        (Nothing, Nothing) -> ("", [])
        (Nothing, o) -> (" LIMIT -1 OFFSET ?", [toPrimitivePersistValue proxy o])
        (l, Nothing) -> (" LIMIT ?", [toPrimitivePersistValue proxy l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrimitivePersistValue proxy l, toPrimitivePersistValue proxy o])
  cond' = renderCond' cond
  fields = renderFields escapeS (constrParams constr)
  mkQuery tname = "SELECT " <> fields <> " FROM " <> escapeS (fromString tname) <> whereClause <> orders <> lim
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  doSelectQuery query cNum = queryRawTyped query types binds $ mapAllRows $ liftM fst . fromEntityPersistValues . (toPrimitivePersistValue proxy cNum:)
  binds = maybe id getValues cond' $ limps
  constr = constructors e !! phantomConstrNum (undefined :: c a)
  types = getConstructorTypes constr

-- | receives constructor number and row of values from the constructor table
mkEntity :: (PersistEntity v, PersistBackend m) => Int -> [PersistValue] -> m (AutoKey v, v)
mkEntity i xs = fromEntityPersistValues (toPrimitivePersistValue proxy i:xs') >>= \(v, _) -> return (k, v) where
  (k, xs') = fromPurePersistValues proxy xs

selectAll' :: forall m v . (MonadBaseControl IO m, MonadIO m, PersistEntity v) => DbPersist Sqlite m [(AutoKey v, v)]
selectAll' = start where
  start = if isSimple (constructors e)
    then let
      constr = head $ constructors e
      fields = maybe id (\key cont -> key <> fromChar ',' <> cont) (constrId constr) $ renderFields escapeS (constrParams constr)
      query = "SELECT " <> fields <> " FROM " <> escapeS (fromString name)
      types = maybe id (const $ (DbInt32:)) (constrId constr) $ getConstructorTypes constr
      in queryRawTyped query types [] $ mapAllRows $ mkEntity 0
    else liftM concat $ forM (zip [0..] (constructors e)) $ \(cNum, constr) -> do
        let fields = fromJust (constrId constr) <> fromChar ',' <> renderFields escapeS (constrParams constr)
        let cName = fromString $ name ++ [delim] ++ constrName constr
        let query = "SELECT " <> fields <> " FROM " <> escapeS cName
        let types = DbInt32:getConstructorTypes constr
        queryRawTyped query types [] $ mapAllRows $ mkEntity cNum

  e = entityDef (undefined :: v)
  name = persistName (undefined :: v)

{-# SPECIALIZE get' :: (PersistEntity v, PrimitivePersistField (Key v BackendSpecific)) => Key v BackendSpecific -> DbPersist Sqlite IO (Maybe v) #-}
get' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
     => Key v BackendSpecific -> DbPersist Sqlite m (Maybe v)
get' (k :: Key v BackendSpecific) = do
  let e = entityDef (undefined :: v)
  let name = persistName (undefined :: v)
  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let fields = renderFields escapeS (constrParams constr)
      let query = "SELECT " <> fields <> "FROM " <> escapeS (fromString name) <> " WHERE " <> fromJust (constrId constr) <> "=?"
      let types = getConstructorTypes constr
      x <- queryRawTyped query types [toPrimitivePersistValue proxy k] id
      case x of
        Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
        Nothing -> return Nothing
    else do
      let query = "SELECT discr FROM " <> escapeS (fromString name) <> " WHERE id=?"
      x <- queryRawTyped query [DbInt32] [toPrimitivePersistValue proxy k] id
      case x of
        Just [discr] -> do
          let constructorNum = fromPrimitivePersistValue proxy discr
          let constr = constructors e !! constructorNum
          let cName = fromString $ name ++ [delim] ++ constrName constr
          let fields = renderFields escapeS (constrParams constr)
          let cQuery = "SELECT " <> fields <> "FROM " <> escapeS cName <> " WHERE " <> fromJust (constrId constr) <> "=?"
          x2 <- queryRawTyped cQuery (getConstructorTypes constr) [toPrimitivePersistValue proxy k] id
          case x2 of
            Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ discr:xs
            Nothing -> fail "Missing entry in constructor table"
        Just x' -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing

{-# SPECIALIZE getBy' :: (PersistEntity v, IsUniqueKey (Key v (Unique u))) => Key v (Unique u) -> DbPersist Sqlite IO (Maybe v) #-}
getBy' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
       => Key v (Unique u) -> DbPersist Sqlite m (Maybe v)
getBy' (k :: Key v (Unique u)) = do
  let e = entityDef (undefined :: v)
  let name = persistName (undefined :: v)
  uniques <- toPersistValues k
  let u = (undefined :: Key v (Unique u) -> u (UniqueMarker v)) k
  let uFields = foldr (renderChain escapeS) [] $ projectionFieldChains u []
  let cond = intercalateS " AND " $ map (<> "=?") uFields
  let constr = head $ constructors e
  let fields = renderFields escapeS (constrParams constr)
  let query = "SELECT " <> fields <> " FROM " <> escapeS (fromString name) <> " WHERE " <> cond
  x <- queryRawTyped query (getConstructorTypes constr) (uniques []) id
  case x of
    Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
    Nothing -> return Nothing
      
update' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => [Update v c] -> Cond v c -> DbPersist Sqlite m ()
update' upds (cond :: Cond v c) = do
  let e = entityDef (undefined :: v)
  let name = persistName (undefined :: v)
  case renderUpdates proxy escapeS upds of
    Just upds' -> do
      let cond' = renderCond' cond
      let mkQuery tname = "UPDATE " <> escapeS tname <> " SET " <> whereClause where
          whereClause = maybe (getQuery upds') (\c -> getQuery upds' <> " WHERE " <> getQuery c) cond'
      let tname = fromString $ if isSimple (constructors e) then name else name ++ [delim] ++ phantomConstrName (undefined :: c a)
      executeRawCached' (mkQuery tname) (getValues upds' <> maybe mempty getValues cond' $ [])
    Nothing -> return ()

delete' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => Cond v c -> DbPersist Sqlite m ()
delete' (cond :: Cond v c) = executeRawCached' query (maybe [] (($ []) . getValues) cond') where
  e = entityDef (undefined :: v)
  constr = head $ constructors e
  cond' = renderCond' cond
  name = persistName (undefined :: v)
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  query = if isSimple (constructors e)
    then "DELETE FROM " <> escapeS (fromString name) <> whereClause
    -- the entries in the constructor table are deleted because of the reference on delete cascade
    else "DELETE FROM " <> escapeS (fromString name) <> " WHERE id IN(SELECT " <> fromJust (constrId constr) <> " FROM " <> escapeS cName <> whereClause <> ")" where
      cName = fromString $ name ++ [delim] ++ phantomConstrName (undefined :: c a)

deleteByKey' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
             => Key v BackendSpecific -> DbPersist Sqlite m ()
deleteByKey' k = executeRawCached' query [toPrimitivePersistValue proxy k] where
  e = entityDef ((undefined :: Key v u -> v) k)
  constr = head $ constructors e
  name = fromString (persistName $ (undefined :: Key v u -> v) k)
  query = "DELETE FROM " <> escapeS name <> " WHERE " <> fromJust (constrId constr) <> "=?"
  

{-# SPECIALIZE count' :: (PersistEntity v, Constructor c) => Cond v c -> DbPersist Sqlite IO Int #-}
count' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => Cond v c -> DbPersist Sqlite m Int
count' (cond :: Cond v c) = do
  let e = entityDef (undefined :: v)
  let cond' = renderCond' cond
  let name = persistName (undefined :: v)
  let tname = fromString $ if isSimple (constructors e)
       then name
       else name ++ [delim] ++ phantomConstrName (undefined :: c a)
  let query = "SELECT COUNT(*) FROM " <> escapeS tname <> whereClause where
      whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  x <- queryRawCached' query (maybe [] (flip getValues []) cond') id
  case x of
    Just [num] -> return $ fromPrimitivePersistValue proxy num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

{-# SPECIALIZE countAll' :: PersistEntity v => v -> DbPersist Sqlite IO Int #-}
countAll' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Sqlite m Int
countAll' (_ :: v) = do
  let name = persistName (undefined :: v)
  let query = "SELECT COUNT(*) FROM " <> escapeS (fromString name)
  x <- queryRawTyped query [DbInt32] [] id
  case x of
    Just [num] -> return $ fromPrimitivePersistValue proxy num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

project' :: forall m v c p opts a' . (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c, Projection p (RestrictionHolder v c) a', HasSelectOptions opts v c)
         => p -> opts -> DbPersist Sqlite m [a']
project' p options = start where
  SelectOptions cond limit offset ords = getSelectOptions options
  start = doSelectQuery $ if isSimple (constructors e)
    then mkQuery name
    else let
      cName = name ++ [delim] ++ constrName constr
      in mkQuery cName

  e = entityDef (undefined :: v)
  orders = renderOrders escapeS ords
  name = persistName (undefined :: v)
  (lim, limps) = case (limit, offset) of
        (Nothing, Nothing) -> ("", [])
        (Nothing, o) -> (" LIMIT -1 OFFSET ?", [toPrimitivePersistValue proxy o])
        (l, Nothing) -> (" LIMIT ?", [toPrimitivePersistValue proxy l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrimitivePersistValue proxy l, toPrimitivePersistValue proxy o])
  cond' = renderCond' cond
  chains = projectionFieldChains p []
  fields = intercalateS (fromChar ',') $ foldr (renderChain escapeS) [] chains
  mkQuery tname = "SELECT " <> fields <> " FROM " <> escapeS (fromString tname) <> whereClause <> orders <> lim
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  doSelectQuery query = queryRawTyped query types binds $ mapAllRows $ liftM fst . projectionResult p
  binds = maybe id getValues cond' $ limps
  constr = constructors e !! phantomConstrNum (undefined :: c a)
  types = foldr getDbTypes [] $ map (snd . fst) chains

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
      PersistInt64 int64     -> S.bindInt64 stmt i int64
      PersistString text     -> S.bindText stmt i text
      PersistDouble double   -> S.bindDouble stmt i double
      PersistBool b          -> S.bindInt64 stmt i $ if b then 1 else 0
      PersistByteString blob -> S.bindBlob stmt i blob
      PersistNull            -> S.bindNull stmt i
      PersistDay d           -> S.bindText stmt i $ show d
      PersistTimeOfDay d     -> S.bindText stmt i $ show d
      PersistUTCTime d       -> S.bindText stmt i $ show d
      PersistZonedTime (ZT d)-> S.bindText stmt i $ show d
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
typeToSqlite (DbMaybe _) = Nothing
typeToSqlite (DbList _ _) = Just S.IntegerColumn
typeToSqlite (DbEntity Nothing _) = Just S.IntegerColumn
typeToSqlite t = error $ "typeToSqlite: DbType does not have corresponding database type: " ++ show t

getConstructorTypes :: ConstructorDef -> [DbType]
getConstructorTypes = foldr getDbTypes [] . map snd . constrParams where

getDbTypes :: DbType -> [DbType] -> [DbType]
getDbTypes typ acc = case typ of
  DbEmbedded (EmbeddedDef _ ts) -> foldr (getDbTypes . snd) acc ts
  DbEntity (Just (EmbeddedDef _ ts, _)) _ -> foldr (getDbTypes . snd) acc ts
  t               -> t:acc

pFromSql :: S.SQLData -> PersistValue
pFromSql (S.SQLInteger i) = PersistInt64 i
pFromSql (S.SQLFloat i)   = PersistDouble i
pFromSql (S.SQLText s)    = PersistString s
pFromSql (S.SQLBlob bs)   = PersistByteString bs
pFromSql (S.SQLNull)      = PersistNull

constrId :: ConstructorDef -> Maybe Utf8
constrId = fmap (escapeS . fromString) . constrAutoKeyName

-- It is used to escape table names and columns, which can include only symbols allowed in Haskell datatypes and '$' delimiter. We need it mostly to support names that coincide with SQL keywords
escape :: String -> String
escape s = '\"' : s ++ "\""

escapeS :: Utf8 -> Utf8
escapeS a = let q = fromChar '"' in q <> a <> q

renderCond' :: (PersistEntity v, Constructor c) => Cond v c -> Maybe (RenderS Utf8)
renderCond' = renderCond proxy escapeS renderEquals renderNotEquals where
  renderEquals a b = a <> " IS " <> b
  renderNotEquals a b = a <> " IS NOT " <> b

isSimple :: [ConstructorDef] -> Bool
isSimple [_] = True
isSimple _   = False

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

compareColumns :: Column Affinity -> Column DbType -> Bool
compareColumns (Column name1 isNull1 aff1 def1) (Column name2 isNull2 type2 def2) =
  aff1 == dbTypeAffinity type2 && name1 == name2 && isNull1 == isNull2 && def1 == def2

compareUniqs :: UniqueDef' -> UniqueDef' -> Bool
compareUniqs (UniqueDef' _ cols1) (UniqueDef' _ cols2) = haveSameElems (==) cols1 cols2

compareRefs :: (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
compareRefs (_, (tbl1, pairs1)) (_, (tbl2, pairs2)) = unescape tbl1 == unescape tbl2 && haveSameElems (==) pairs1 pairs2 where
  unescape name = if head name == '"' && last name == '"' then tail $ init name else name

-- from database, from datatype
migrateUniq :: (UniqueDef', UniqueDef') -> [AlterTable]
migrateUniq (UniqueDef' name cols, UniqueDef' name' cols') = if haveSameElems (==) cols cols'
  then []
  else [DropConstraint name, AddUniqueConstraint name' cols']

-- from database, from datatype
migrateColumn :: (Column Affinity, Column DbType) -> [AlterColumn']
migrateColumn (Column name1 isNull1 aff1 def1, Column _ isNull2 type2 def2) = modDef ++ modNull ++ modType where
  modNull = case (isNull1, isNull2) of
    (False, True) -> [(name1, IsNull)]
    (True, False) -> case def2 of
      Nothing -> [(name1, NotNull)]
      Just s -> [(name1, UpdateValue s), (name1, NotNull)]
    _ -> []
  modType = if aff1 == dbTypeAffinity type2 then [] else [(name1, Type type2)]
  modDef = if def1 == def2
    then []
    else [(name1, maybe NoDefault Default def2)]

mainTableId :: String
mainTableId = "id"

showAlterDb :: AlterDB Affinity -> SingleMigration
showAlterDb (AddTable s) = Right [(False, defaultPriority, s)]
showAlterDb (AlterTable table createTable (oldId, oldCols, _, _) (newId, newCols, _, _) alts) | not (all isSupported alts) = (Right
  [ (False, defaultPriority, "CREATE TEMP TABLE " ++ escape tableTmp ++ "(" ++ columnsTmp ++ ")")
  , (False, defaultPriority, copy (table, columnsTmp) (tableTmp, columnsTmp))
  , (not (null oldOnlyColumns), defaultPriority, "DROP TABLE " ++ escape table)
  , (False, defaultPriority, createTable)
  , (False, defaultPriority, copy (tableTmp, columnsTmp) (table, columnsNew))
  , (False, defaultPriority, "DROP TABLE " ++ escape tableTmp)
  ]) where
    tableTmp = table ++ "_backup"
    copy (from, fromCols) (to, toCols) = "INSERT INTO " ++ escape to ++ "(" ++ toCols ++ ") SELECT " ++ fromCols ++ " FROM " ++ escape from
    (oldOnlyColumns, _, commonColumns) = matchElements compareColumns oldCols newCols
    columnsTmp = intercalate "," $ map escape $ maybeToList (newId >> oldId) ++ map (colName . snd) commonColumns
    columnsNew = intercalate "," $ map escape $ maybeToList (oldId >> newId) ++ map (colName . snd) commonColumns
    isSupported (AlterColumn (_, Add _)) = True
    isSupported (AlterColumn (_, UpdateValue _)) = True
    isSupported _ = False
showAlterDb (AlterTable t _ _ _ alts) = Right $ map (showAlterTable t) alts
showAlterDb (DropTrigger name _) = Right [(False, triggerPriority, "DROP TRIGGER " ++ escape name)]
showAlterDb (AddTriggerOnDelete trigName tableName body) = Right [(False, triggerPriority,
  "CREATE TRIGGER " ++ escape trigName ++ " DELETE ON " ++ escape tableName ++ " BEGIN " ++ body ++ "END")]
showAlterDb (AddTriggerOnUpdate trigName tableName fieldName body) = Right [(False, triggerPriority,
  "CREATE TRIGGER " ++ escape trigName ++ " UPDATE OF " ++ escape fieldName ++ " ON " ++ escape tableName ++ " BEGIN " ++ body ++ "END")]
showAlterDb alt = error $ "showAlterDb: does not support " ++ show alt

showAlterTable :: String -> AlterTable -> (Bool, Int, String)
showAlterTable table (AlterColumn alt) = showAlterColumn table alt
showAlterTable table alt = error $ "showAlterTable: does not support " ++ show alt ++ " (table " ++ table ++ ")"

showAlterColumn :: String -> AlterColumn' -> (Bool, Int, String)
showAlterColumn table (_, Add col) = (False, defaultPriority, concat
    [ "ALTER TABLE "
    , escape table
    , " ADD COLUMN "
    , showColumn col
    ])
showAlterColumn table (n, UpdateValue s) = (False, defaultPriority, concat
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
showAlterColumn table alt = error $ "showAlterColumn: does not support " ++ show alt ++ " (table " ++ table ++ ")"
