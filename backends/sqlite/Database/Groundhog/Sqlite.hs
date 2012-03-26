{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
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
import Database.Groundhog.Generic hiding (cName)
import Database.Groundhog.Generic.Sql.String

import qualified Database.Sqlite as S

import Control.Arrow ((&&&))
import Control.Monad(liftM, forM, (>=>))
import Control.Monad.Trans.Control(MonadBaseControl)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader(ask)
import Data.Int (Int64)
import Data.List (group, intercalate)
import Data.IORef
import qualified Data.Map as Map
import Data.Monoid
import Data.Pool

-- typical operations for connection: OPEN, BEGIN, COMMIT, ROLLBACK, CLOSE
data Sqlite = Sqlite S.Database (IORef (Map.Map String S.Statement))

instance (MonadBaseControl IO m, MonadIO m) => PersistBackend (DbPersist Sqlite m) where
  {-# SPECIALIZE instance PersistBackend (DbPersist Sqlite IO) #-}
  insert v = insert' v
  insertBy v = insertBy' v
  replace k v = replace' k v
  select cond ords limit offset = select' cond ords limit offset
  selectAll = selectAll'
  get k = get' k
  update upds cond = update' upds cond
  delete cond = delete' cond
  deleteByKey k = deleteByKey' k
  count cond = count' cond
  countAll fakeV = countAll' fakeV
  migrate fakeV = migrate' fakeV

  executeRaw False query ps = executeRaw' query ps
  executeRaw True query ps = executeRawCached' query ps
  queryRaw False query ps f = queryRaw' query ps f
  queryRaw True query ps f = queryRawCached' query ps f

  insertList l = insertList' l
  getList k = getList' k

--{-# SPECIALIZE withSqlitePool :: String -> Int -> (Pool Sqlite -> IO a) -> IO a #-}
withSqlitePool :: (MonadBaseControl IO m, MonadIO m)
               => String
               -> Int -- ^ number of connections to open
               -> (Pool Sqlite -> m a)
               -> m a
withSqlitePool s = createPool (open' s) close'

{-# SPECIALIZE withSqliteConn :: String -> (Sqlite -> IO a) -> IO a #-}
{-# INLINE withSqliteConn #-}
withSqliteConn :: (MonadBaseControl IO m, MonadIO m)
               => String
               -> (Sqlite -> m a)
               -> m a
withSqliteConn s = bracket (liftIO $ open' s) (liftIO . close')

{-# SPECIALIZE runSqlitePool :: DbPersist Sqlite IO a -> Pool Sqlite -> IO a #-}
runSqlitePool :: (MonadBaseControl IO m, MonadIO m) => DbPersist Sqlite m a -> Pool Sqlite -> m a
runSqlitePool = flip withPool' . runSqliteConn

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
    let name = getEntityName e
    let constrs = constructors e
    let mainTableQuery = "CREATE TABLE " ++ escape name ++ " (id$ INTEGER PRIMARY KEY, discr$ INTEGER NOT NULL)"
    if isSimple constrs
      then do
        x <- checkTable name
        -- check whether the table was created for multiple constructors before
        case x of
          Just sql | sql == mainTableQuery -> do
            return $ Left ["Datatype with multiple constructors was truncated to one constructor. Manual migration required. Datatype: " ++ name]
          _ -> liftM snd $ migConstrAndTrigger True name $ head constrs
      else do
        mainsql <- checkTable name
        let constrTable c = name ++ [defDelim] ++ constrName c
        res <- mapM (\c -> migConstrAndTrigger False name c) constrs
        case mainsql of
          Nothing -> do
            -- no constructor tables can exist if there is no main data table
            let orphans = filter (fst.fst) $ zip res constrs
            return $ if null orphans
              then mergeMigrations $ Right [(False, defaultPriority, mainTableQuery)]:map snd res
              else Left $ map (\(_, c) -> "Orphan constructor table found: " ++ constrTable c) orphans
          Just sql -> do
            if sql == mainTableQuery
              then do
                -- the datatype had also many constructors before
                -- check whether any new constructors appeared and increment older discriminators, which were shifted by newer constructors inserted not in the end
                let updateDiscriminators = Right . go 0 . map (head &&& length) . group . map fst $ res where
                    go acc ((False, n):(True, n2):xs) = (False, defaultPriority, "UPDATE " ++ escape name ++ " SET discr$ = discr$ + " ++ show n ++ " WHERE discr$ >= " ++ show acc) : go (acc + n + n2) xs
                    go acc ((True, n):xs) = go (acc + n) xs
                    go _ _ = []
                return $ mergeMigrations $ updateDiscriminators: (map snd res)
              else do
                return $ Left ["Migration from one constructor to many will be implemented soon. Datatype: " ++ name]
  migL t = do
    let mainName = "List$" ++ "$" ++ getName t
    let valuesName = mainName ++ "$" ++ "values"
    let valueCols = mkColumns "value" t
    let mainQuery = "CREATE TABLE " ++ mainName ++ " (id$ INTEGER PRIMARY KEY)"
    let valuesQuery = "CREATE TABLE " ++ valuesName ++ " (id$ INTEGER REFERENCES " ++ mainName ++ ", ord$ INTEGER NOT NULL" ++ concatMap sqlColumn valueCols ++ ")"
    x <- checkTable mainName
    y <- checkTable valuesName
    let triggerMain = Right []
--    (_, triggerMain) <- migTriggerOnDelete mainName ["DELETE FROM " ++ valuesName ++ " WHERE id$=old.id$;"]
    (_, triggerValues) <- migTriggerOnDelete valuesName $ map snd $ mkDeletes valueCols
    let f name a b = if a /= b then ["List table " ++ name ++ " error. Expected: " ++ a ++ ". Found: " ++ b] else []
    return $ case (x, y) of
      (Nothing, Nothing) -> mergeMigrations [Right [(False, defaultPriority, mainQuery), (False, defaultPriority, valuesQuery)], triggerMain, triggerValues]
      (Just sql1, Just sql2) -> let errors = f mainName mainQuery sql1 ++ f valuesName valuesQuery sql2
                                in if null errors then Right [] else Left errors
      (_, Nothing) -> Left ["Found orphan main list table " ++ mainName]
      (Nothing, _) -> Left ["Found orphan list values table " ++ valuesName]

migConstrAndTrigger :: (MonadBaseControl IO m, MonadIO m) => Bool -> String -> ConstructorDef -> DbPersist Sqlite m (Bool, SingleMigration)
migConstrAndTrigger simple name constr = do
  let cName = if simple then name else name ++ [defDelim] ++ constrName constr
  (constrExisted, mig) <- migConstr (if simple then Nothing else Just name) cName constr
  let columns = concatMap (uncurry mkColumns) $ constrParams constr
  let dels = mkDeletes columns
  (triggerExisted, delTrigger) <- migTriggerOnDelete cName (map snd dels)
  updTriggers <- mapM (liftM snd . uncurry (migTriggerOnUpdate cName)) dels
  return $ if constrExisted == triggerExisted || (constrExisted && null dels)
    then (constrExisted, mergeMigrations ([mig, delTrigger] ++ updTriggers))
    -- this can happen when an ephemeral field was added. Consider doing something else except throwing an error
    else (constrExisted, Left ["Both trigger and constructor table must exist: " ++ cName])

migConstr :: (MonadBaseControl IO m, MonadIO m) => Maybe String -> String -> ConstructorDef -> DbPersist Sqlite m (Bool, SingleMigration)
migConstr mainTableName cName constr = do
  let fields = concatMap (uncurry mkColumns) $ constrParams constr
  let uniques = constrConstrs constr
  let mainRef = maybe "" (\x -> " REFERENCES " ++ escape x ++ " ON DELETE CASCADE ") mainTableName
  let query = "CREATE TABLE " ++ escape cName ++ " (" ++ constrId ++ " INTEGER PRIMARY KEY" ++ mainRef ++ concatMap sqlColumn fields ++ concatMap sqlUnique uniques ++ ")"
  x <- checkTable cName
  return $ case x of
    Nothing  -> (False, Right [(False, defaultPriority, query)])
    Just sql -> (True, if sql == query
      then Right []
      else Left ["Constructor table must be altered: " ++ cName])

-- it handles only delete operations. So far when list or tuple replace is not allowed, it is ok
migTriggerOnDelete :: (MonadBaseControl IO m, MonadIO m) => String -> [String] -> DbPersist Sqlite m (Bool, SingleMigration)
migTriggerOnDelete name deletes = do
  let query = "CREATE TRIGGER " ++ escape name ++ " DELETE ON " ++ escape name ++ " BEGIN " ++ concat deletes ++ "END"
  x <- checkTrigger name
  return $ case x of
    Nothing | null deletes -> (False, Right [])
    Nothing -> (False, Right [(False, triggerPriority, query)])
    Just sql -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
      then Right [(False, triggerPriority, "DROP TRIGGER " ++ escape name)]
      else if sql == query
        then Right []
        -- this can happen when a field was added or removed. Consider trigger replacement.
        else Left ["The trigger " ++ name ++ " is different from expected. Manual migration required.\n" ++ sql ++ "\n" ++ query])
        
-- | Table name and a  list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: (MonadBaseControl IO m, MonadIO m) => String -> String -> String -> DbPersist Sqlite m (Bool, SingleMigration)
migTriggerOnUpdate name fieldName del = do
  let tname = name ++ "$" ++ fieldName
  let query = "CREATE TRIGGER " ++ escape tname ++ " UPDATE OF " ++ escape fieldName ++ " ON " ++ escape name ++ " BEGIN " ++ del ++ "END"
  x <- checkTrigger tname
  return $ case x of
    Nothing -> (False, Right [(False, triggerPriority, query)])
    Just sql -> (True, if sql == query
        then Right []
        else Left ["The trigger " ++ tname ++ " is different from expected. Manual migration required.\n" ++ sql ++ "\n" ++ query])
  
-- on delete removes all ephemeral data
-- returns column name and delete statement for the referenced table
mkDeletes :: [Column] -> [(String, String)]
mkDeletes columns = map delField ephemerals where
  delField (Column name _ t _ (Just ref)) = (name, maybe "" id delVal ++ delMain) where
    delMain = "DELETE FROM " ++ ref ++ " WHERE id$=old." ++ escape name ++ ";"
    delVal = case t of DbList _ -> Just ("DELETE FROM " ++ ref ++ "$values" ++ " WHERE id$=old." ++ escape name ++ ";"); _ -> Nothing
  ephemerals = filter (isEphemeral . cType) columns

isEphemeral :: DbType -> Bool
isEphemeral (DbMaybe x) = isEphemeral (getType x)
isEphemeral (DbList _) = True
isEphemeral _ = False

checkTrigger :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Sqlite m (Maybe String)
checkTrigger = checkSqliteMaster "trigger"

checkTable :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Sqlite m (Maybe String)
checkTable = checkSqliteMaster "table"

checkSqliteMaster :: (MonadBaseControl IO m, MonadIO m) => String -> String -> DbPersist Sqlite m (Maybe String)
checkSqliteMaster vtype name = do
  let query = "SELECT sql FROM sqlite_master WHERE type = ? AND name = ?"
  x <- queryRawTyped query [DbString] [toPrim vtype, toPrim name] firstRow
  let throwErr = error . ("Unexpected result from sqlite_master: " ++)
  case x of
    Nothing -> return Nothing
    Just [hsql] -> case hsql of
      PersistString sql -> return $ Just sql
      err               -> throwErr $ "column sql is not string: " ++ show err
    Just xs -> throwErr $ "requested 1 column, returned " ++ show xs

getStatementCached :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Sqlite m S.Statement
getStatementCached sql = do
  Sqlite conn smap <- DbPersist ask
  liftIO $ do
    smap' <- readIORef smap
    case Map.lookup sql smap' of
      Nothing -> do
        stmt <- S.prepare conn sql
        writeIORef smap (Map.insert sql stmt smap')
        return stmt
      Just stmt -> return stmt

getStatement :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Sqlite m S.Statement
getStatement sql = do
  Sqlite conn _ <- DbPersist ask
  liftIO $ S.prepare conn sql

showSqlType :: DbType -> String
showSqlType DbString = "VARCHAR"
showSqlType DbInt32 = "INTEGER"
showSqlType DbInt64 = "INTEGER"
showSqlType DbReal = "REAL"
showSqlType DbBool = "BOOLEAN"
showSqlType DbDay = "DATE"
showSqlType DbTime = "TIME"
showSqlType DbDayTime = "TIMESTAMP"
showSqlType DbBlob = "BLOB"
showSqlType (DbMaybe t) = showSqlType (getType t)
showSqlType (DbList _) = "INTEGER"
showSqlType (DbEntity _) = "INTEGER"
showSqlType t@(DbEmbedded _ _) = error $ "showSqlType: DbType does not have corresponding database type: " ++ show t

{-
DbMaybe prim -> name type
prim         -> name type NOT NULL
comp         -> name type NOT NULL REFERENCES table
DbMaybe comp -> name type REFERENCES table
-}
sqlColumn :: Column -> String
sqlColumn (Column name isNull typ _ ref) = ", " ++ escape name ++ " " ++ showSqlType typ ++ rest where
  rest = case (isNull, ref) of
    (False, Nothing) -> " NOT NULL"
    (False, Just ref') -> " NOT NULL REFERENCES " ++ escape ref'
    (True, Nothing) -> ""
    (True, Just ref') -> " REFERENCES " ++ escape ref' ++ " ON DELETE SET NULL"

sqlUnique :: Constraint -> String
sqlUnique (cname, cols) = concat
    [ ", CONSTRAINT "
    , escape cname
    , " UNIQUE ("
    , intercalate "," $ map escape cols
    , ")"
    ]

{-allColumns :: ConstructorDef -> String
allColumns constr = go "" $ constrParams constr where
  go :: String -> [(String, NamedType)] -> String
  go acc [] = acc
  go acc ((name, typ):xs) = case typ of
    DbTuple _ ts -> 
    _ -> (name ++) . acc
-}
{-# SPECIALIZE insert' :: PersistEntity v => v -> DbPersist Sqlite IO (Key v) #-}
{-# INLINE insert' #-}
insert' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> DbPersist Sqlite m (Key v)
insert' v = do
  -- constructor number and the rest of the field values
  vals <- toEntityPersistValues v
  let e = entityDef v
  let name = getEntityName e
  let constructorNum = fromPrim (head vals)

  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let query = insertIntoConstructorTable False name constr
      executeRawCached' query (tail vals)
      rowid <- getLastInsertRowId
      return $ Key rowid
    else do
      let constr = constructors e !! constructorNum
      let cName = name ++ [defDelim] ++ constrName constr
      let query = "INSERT INTO " ++ escape name ++ "(discr$)VALUES(?)"
      executeRawCached' query $ take 1 vals
      rowid <- getLastInsertRowId
      let cQuery = insertIntoConstructorTable True cName constr
      executeRawCached' cQuery $ PersistInt64 rowid:(tail vals)
      return $ Key rowid

-- in Sqlite we can insert null to the id column. If so, id will be generated automatically
insertIntoConstructorTable :: Bool -> String -> ConstructorDef -> String
insertIntoConstructorTable withId tName c = "INSERT INTO " ++ escape tName ++ "(" ++ fieldNames ++ ")VALUES(" ++ placeholders ++ ")" where
  fields = if withId then (constrId, namedType (0 :: Int64)):constrParams c else constrParams c
  fieldNames   = fromStringS (renderFields escapeS fields) ""
  placeholders = fromStringS (renderFields (const $ fromChar '?') fields) ""

{-# SPECIALIZE insertBy' :: PersistEntity v => v -> DbPersist Sqlite IO (Either (Key v) (Key v)) #-}
insertBy' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Sqlite m (Either (Key v) (Key v))
insertBy' v = do
  let e = entityDef v
  let name = getEntityName e

  let constraints = getConstraints v
  let constructorNum = fst constraints
  let constraintFields = map snd $ snd constraints
  let constrCond = intercalate " OR " $ map (intercalate " AND " . map (\(fname, _) -> escape fname ++ "=?")) constraintFields

  let ifAbsent tname ins = if null constraintFields
       then liftM (Right . Key) ins
       else do
         let query = "SELECT " ++ constrId ++ " FROM " ++ escape tname ++ " WHERE " ++ constrCond
         x <- queryRawTyped query [DbInt64] (concatMap (map snd) constraintFields) firstRow
         case x of
           Nothing  -> liftM (Right . Key) ins
           Just [k] -> return $ Left $ fromPrim k
           Just xs  -> fail $ "unexpected query result: " ++ show xs

  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      ifAbsent name $ do
        let query = insertIntoConstructorTable False name constr
        vals <- toEntityPersistValues v
        executeRawCached' query (tail vals)
        getLastInsertRowId
    else do
      let constr = constructors e !! constructorNum
      let cName = name ++ [defDelim] ++ constrName constr
      ifAbsent cName $ do
        let query = "INSERT INTO " ++ escape name ++ "(discr$)VALUES(?)"
        vals <- toEntityPersistValues v
        executeRawCached' query $ take 1 vals
        rowid <- getLastInsertRowId
        let cQuery = insertIntoConstructorTable True cName constr
        executeRawCached' cQuery $ PersistInt64 rowid :(tail vals)
        return rowid

replace' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => Key v -> v -> DbPersist Sqlite m ()
replace' k v = do
  vals <- toEntityPersistValues v
  let e = entityDef v
  let name = getEntityName e
  let constructorNum = fromPrim (head vals)
  let constr = constructors e !! constructorNum

  let upds = fromStringS (renderFields (\f -> f <> "=?") $ constrParams constr) ""
  let mkQuery tname = "UPDATE " ++ escape tname ++ " SET " ++ upds ++ " WHERE " ++ constrId ++ "=?"

  if isSimple (constructors e)
    then executeRawCached' (mkQuery name) (tail vals ++ [toPrim k])
    else do
      let query = "SELECT discr$ FROM " ++ escape name ++ " WHERE id$=?"
      x <- queryRawTyped query [DbInt32] [toPrim k] (firstRow >=> return . fmap (fromPrim . head))
      case x of
        Just discr -> do
          let cName = name ++ [defDelim] ++ constrName constr

          if discr == constructorNum
            then executeRaw True (mkQuery cName) (tail vals ++ [toPrim k])
            else do
              let insQuery = insertIntoConstructorTable True cName constr
              executeRawCached' insQuery (toPrim k:tail vals)

              let oldCName = name ++ [defDelim] ++ constrName (constructors e !! discr)
              let delQuery = "DELETE FROM " ++ escape oldCName ++ " WHERE " ++ constrId ++ "=?"
              executeRawCached' delQuery [toPrim k]

              let updateDiscrQuery = "UPDATE " ++ escape name ++ " SET discr$=? WHERE id$=?"
              executeRaw True updateDiscrQuery [head vals, toPrim k]
        Nothing -> return ()

-- | receives constructor number and row of values from the constructor table
mkEntity :: (PersistEntity v, PersistBackend m) => Int -> [PersistValue] -> m (Key v, v)
mkEntity i (k:xs) = fromEntityPersistValues (toPrim i:xs) >>= \v -> return (fromPrim k, v)
mkEntity _ [] = error "Unable to create entity. No values supplied"

select' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => Cond v c -> [Order v c] -> Int -> Int -> DbPersist Sqlite m [(Key v, v)]
select' (cond :: Cond v c) ords limit offset = start where
  start = if isSimple (constructors e)
    then doSelectQuery (mkQuery name) 0
    else let
      cName = name ++ [defDelim] ++ constrName constr
      in doSelectQuery (mkQuery cName) $ constrNum constr

  e = entityDef (undefined :: v)
  orders = renderOrders escapeS ords
  name = getEntityName e
  (lim, limps) = case (limit, offset) of
        (0, 0) -> ("", [])
        (0, o) -> (" LIMIT -1 OFFSET ?", [toPrim o])
        (l, 0) -> (" LIMIT ?", [toPrim l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrim l, toPrim o])
  cond' = renderCond' cond
  mkQuery tname = "SELECT * FROM " ++ escape tname ++ fromStringS (whereClause <> orders <> lim) ""
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  doSelectQuery query cNum = queryRawTyped query types binds $ mapAllRows (mkEntity cNum)
  binds = maybe id getValues cond' $ limps
  constr = constructors e !! phantomConstrNum (undefined :: c)
  types = DbInt64:getConstructorTypes constr

selectAll' :: forall m v.(MonadBaseControl IO m, MonadIO m, PersistEntity v) => DbPersist Sqlite m [(Key v, v)]
selectAll' = start where
  start = if isSimple (constructors e)
    then let
      query = "SELECT * FROM " ++ escape name
      types = DbInt64:(getConstructorTypes $ head $ constructors e)
      in queryRawTyped query types [] $ mapAllRows (mkEntity 0)
    else liftM concat $ forM (zip [0..] (constructors e)) $ \(i, constr) -> do
        let cName = name ++ [defDelim] ++ constrName constr
        let query = "SELECT * FROM " ++ escape cName
        let types = DbInt64:getConstructorTypes constr
        queryRawTyped query types [] $ mapAllRows (mkEntity i)

  e = entityDef (undefined :: v)
  name = getEntityName e

{-# SPECIALIZE get' :: PersistEntity v => Key v -> DbPersist Sqlite IO (Maybe v) #-}
get' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => Key v -> DbPersist Sqlite m (Maybe v)
get' (k :: Key v) = do
  let e = entityDef (undefined :: v)
  let name = getEntityName e
  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let fields = fromString constrId <> fromChar ',' <> renderFields escapeS (constrParams constr)
      let query = fromStringS ("SELECT " <> fields) $ "FROM " ++ escape name ++ " WHERE " ++ constrId ++ "=?"
      x <- queryRawTyped query (DbInt64:getConstructorTypes constr) [toPrim k] firstRow
      case x of
        Just (_:xs) -> liftM Just $ fromEntityPersistValues $ PersistInt64 0:xs
        Just x'    -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing
    else do
      let query = "SELECT discr$ FROM " ++ escape name ++ " WHERE id$=?"
      x <- queryRawTyped query [DbInt64] [toPrim k] firstRow
      case x of
        Just [discr] -> do
          let constructorNum = fromPrim discr
          let constr = constructors e !! constructorNum
          let cName = name ++ [defDelim] ++ constrName constr
          let fields = fromString constrId <> fromChar ',' <> renderFields escapeS (constrParams constr)
          let cQuery = fromStringS ("SELECT " <> fields) $ "FROM " ++ escape cName ++ " WHERE " ++ constrId ++ "=?"
          x2 <- queryRawTyped cQuery (DbInt64:getConstructorTypes constr) [toPrim k] firstRow
          case x2 of
            Just (_:xs) -> liftM Just $ fromEntityPersistValues $ discr:xs
            Just x2'    -> fail $ "Unexpected number of columns returned: " ++ show x2'
            Nothing     -> fail "Missing entry in constructor table"
        Just x' -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing

update' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => [Update v c] -> Cond v c -> DbPersist Sqlite m ()
update' upds (cond :: Cond v c) = do
  let e = entityDef (undefined :: v)
  let name = getEntityName e
  case renderUpdates escapeS upds of
    Just upds' -> do
      let cond' = renderCond' cond
      let mkQuery tname = "UPDATE " ++ escape tname ++ " SET " ++ fromStringS whereClause "" where
          whereClause = maybe (getQuery upds') (\c -> getQuery upds' <> " WHERE " <> getQuery c) cond'
      let qName = if isSimple (constructors e) then name else name ++ [defDelim] ++ phantomConstrName (undefined :: c)
      executeRawCached' (mkQuery qName) (getValues upds' <> maybe mempty getValues cond' $ [])
    Nothing -> return ()

delete' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => Cond v c -> DbPersist Sqlite m ()
delete' (cond :: Cond v c) = executeRawCached' query (maybe [] (($ []) . getValues) cond') where
  e = entityDef (undefined :: v)
  cond' = renderCond' cond
  name = getEntityName e
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  query = if isSimple (constructors e)
    then "DELETE FROM " ++ escape name ++ fromStringS whereClause ""
    -- the entries in the constructor table are deleted because of the reference on delete cascade
    else "DELETE FROM " ++ escape name ++ " WHERE id$ IN(SELECT id$ FROM " ++ escape cName ++ fromStringS whereClause ")" where
      cName = name ++ [defDelim] ++ phantomConstrName (undefined :: c)

deleteByKey' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => Key v -> DbPersist Sqlite m ()
deleteByKey' (k :: Key v) = do
  let e = entityDef (undefined :: v)
  let name = getEntityName e
  let query = "DELETE FROM " ++ escape name ++ " WHERE id$=?"
  executeRawCached' query [toPrim k]

{-# SPECIALIZE count' :: (PersistEntity v, Constructor c) => Cond v c -> DbPersist Sqlite IO Int #-}
count' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => Cond v c -> DbPersist Sqlite m Int
count' (cond :: Cond v c) = do
  let e = entityDef (undefined :: v)
  let cond' = renderCond' cond
  let name = getEntityName e
  let tname = if isSimple (constructors e)
       then name
       else name ++ [defDelim] ++ phantomConstrName (undefined :: c)
  let query = "SELECT COUNT(*) FROM " ++ escape tname ++ fromStringS whereClause "" where
      whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  x <- queryRawCached' query (maybe [] (($ []) . getValues) cond') firstRow
  case x of
    Just [num] -> return $ fromPrim num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

{-# SPECIALIZE countAll' :: PersistEntity v => v -> DbPersist Sqlite IO Int #-}
countAll' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Sqlite m Int
countAll' (_ :: v) = do
  let name = persistName (undefined :: v)
  let query = "SELECT COUNT(*) FROM " ++ name
  x <- queryRawTyped query [DbInt64] [] firstRow
  case x of
    Just [num] -> return $ fromPrim num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

insertList' :: forall m a.(MonadBaseControl IO m, MonadIO m, PersistField a) => [a] -> DbPersist Sqlite m Int64
insertList' l = do
  let mainName = "List$$" ++ persistName (undefined :: a)
  executeRaw True ("INSERT INTO " ++ mainName ++ " DEFAULT VALUES") []
  k <- getLastInsertRowId
  let valuesName = mainName ++ "$" ++ "values"
  let fields = [("ord$", namedType (0 :: Int)), ("value", namedType (undefined :: a))]
  let query = "INSERT INTO " ++ escape valuesName ++ "(id$," ++ fromStringS (renderFields escapeS fields <> ")VALUES(?," <> renderFields (const $ fromChar '?') fields) ")"
  let go :: Int -> [a] -> DbPersist Sqlite m ()
      go n (x:xs) = do
       x' <- toPersistValues x
       executeRaw True query $ (toPrim k:) . (toPrim n:) . x' $ []
       go (n + 1) xs
      go _ [] = return ()
  go 0 l
  return k
  
getList' :: forall m a.(MonadBaseControl IO m, MonadIO m, PersistField a) => Int64 -> DbPersist Sqlite m [a]
getList' k = do
  let mainName = "List$$" ++ persistName (undefined :: a)
  let valuesName = mainName ++ "$values"
  let value = ("value", namedType (undefined :: a))
  let query = fromStringS ("SELECT " <> renderFields escapeS [value] <> " FROM " <> escapeS (fromString valuesName)) " WHERE id$=? ORDER BY ord$"
  queryRawTyped query (getDbTypes (namedType (undefined :: a)) []) [toPrim k] $ mapAllRows (liftM fst . fromPersistValues)
    
{-# SPECIALIZE getLastInsertRowId :: DbPersist Sqlite IO Int64 #-}
getLastInsertRowId :: (MonadBaseControl IO m, MonadIO m) => DbPersist Sqlite m Int64
getLastInsertRowId = do
  stmt <- getStatementCached "SELECT last_insert_rowid()"
  liftIO $ flip finally (liftIO $ S.reset stmt) $ do
    S.step stmt
    x <- S.column stmt 0
    return $ fromPrim $ pFromSql x

constrId :: String
constrId = defId

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
    go (i + 1) xs

executeRaw' :: (MonadBaseControl IO m, MonadIO m) => String -> [PersistValue] -> DbPersist Sqlite m ()
executeRaw' query vals = do
  stmt <- getStatement query
  liftIO $ flip finally (S.finalize stmt) $ do
    bind stmt vals
    S.Done <- S.step stmt
    return ()

{-# SPECIALIZE executeRawCached' :: String -> [PersistValue] -> DbPersist Sqlite IO () #-}
executeRawCached' :: (MonadBaseControl IO m, MonadIO m) => String -> [PersistValue] -> DbPersist Sqlite m ()
executeRawCached' query vals = do
  stmt <- getStatementCached query
  liftIO $ flip finally (S.reset stmt) $ do
    bind stmt vals
    S.Done <- S.step stmt
    return ()

queryRaw' :: (MonadBaseControl IO m, MonadIO m) => String -> [PersistValue] -> (RowPopper (DbPersist Sqlite m) -> DbPersist Sqlite m a) -> DbPersist Sqlite m a
queryRaw' query vals f = do
  stmt <- getStatement query
  flip finally (liftIO $ S.finalize stmt) $ do
    liftIO $ bind stmt vals
    f $ liftIO $ do
      x <- S.step stmt
      case x of
        S.Done -> return Nothing
        S.Row  -> liftM (Just . map pFromSql) $ S.columns stmt

queryRawCached' :: (MonadBaseControl IO m, MonadIO m) => String -> [PersistValue] -> (RowPopper (DbPersist Sqlite m) -> DbPersist Sqlite m a) -> DbPersist Sqlite m a
queryRawCached' query vals f = do
  stmt <- getStatementCached query
  flip finally (liftIO $ S.reset stmt) $ do
    liftIO $ bind stmt vals
    f $ liftIO $ do
      x <- S.step stmt
      case x of
        S.Done -> return Nothing
        S.Row  -> fmap (Just . map pFromSql) $ S.columns stmt

queryRawTyped :: (MonadBaseControl IO m, MonadIO m) => String -> [DbType] -> [PersistValue] -> (RowPopper (DbPersist Sqlite m) -> DbPersist Sqlite m a) -> DbPersist Sqlite m a
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
typeToSqlite DbBlob = Just S.BlobColumn
typeToSqlite (DbMaybe _) = Nothing
typeToSqlite (DbList _) = Just S.IntegerColumn
typeToSqlite t@(DbEmbedded _ _) = error $ "typeToSqlite: DbType does not have corresponding database type: " ++ show t
typeToSqlite (DbEntity _) = Just S.IntegerColumn

getConstructorTypes :: ConstructorDef -> [DbType]
getConstructorTypes = foldr getDbTypes [] . map snd . constrParams where

getDbTypes :: NamedType -> [DbType] -> [DbType]
getDbTypes typ acc = case getType typ of
  DbEmbedded _ ts -> foldr (getDbTypes . snd) acc ts
  t               -> t:acc

firstRow :: Monad m => RowPopper m -> m (Maybe [PersistValue])
firstRow pop = pop >>= return

mapAllRows :: Monad m => ([PersistValue] -> m a) -> RowPopper m -> m [a]
mapAllRows f pop = go where
  go = pop >>= maybe (return []) (f >=> \a -> liftM (a:) go)

pFromSql :: S.SQLData -> PersistValue
pFromSql (S.SQLInteger i) = PersistInt64 i
pFromSql (S.SQLFloat i)   = PersistDouble i
pFromSql (S.SQLText s)    = PersistString s
pFromSql (S.SQLBlob bs)   = PersistByteString bs
pFromSql (S.SQLNull)      = PersistNull

-- It is used to escape table names and columns, which can include only symbols allowed in Haskell datatypes and '$' delimiter. We need it mostly to support names that coincide with SQL keywords
escape :: String -> String
escape s = '\"' : s ++ "\""

escapeS :: StringS -> StringS
escapeS a = let q = fromChar '"' in q <> a <> q

renderCond' :: (PersistEntity v, Constructor c) => Cond v c -> Maybe (RenderS StringS)
renderCond' = renderCond escapeS constrId renderEquals renderNotEquals where
  renderEquals a b = a <> " IS " <> b
  renderNotEquals a b = a <> " IS NOT " <> b

isSimple :: [ConstructorDef] -> Bool
isSimple [_] = True
isSimple _   = False

defaultPriority :: Int
defaultPriority = 0

triggerPriority :: Int
triggerPriority = 1
