{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
module Database.Groundhog.Postgresql
    ( withPostgresqlPool
    , withPostgresqlConn
    , runPostgresqlPool
    , runPostgresqlConn
    , Postgresql
    , module Database.Groundhog
    ) where

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic hiding (cName)
import Database.Groundhog.Generic.Sql.String
import Database.Groundhog.Postgresql.Base
import Database.Groundhog.Postgresql.Migration

import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as H

import Control.Monad (liftM, forM, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control(MonadBaseControl)
import Control.Monad.Trans.Reader (ask)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as Map
import Data.Monoid
import Data.Pool

instance (MonadBaseControl IO m, MonadIO m) => PersistBackend (DbPersist Postgresql m) where
  {-# SPECIALIZE instance PersistBackend (DbPersist Postgresql IO) #-}
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

--{-# SPECIALIZE withPostgresqlPool :: String -> Int -> (Pool Postgresql -> IO a) -> IO a #-}
withPostgresqlPool :: (MonadBaseControl IO m, MonadIO m)
               => String
               -> Int -- ^ number of connections to open
               -> (Pool Postgresql -> m a)
               -> m a
withPostgresqlPool s = createPool (open' s) close'

{-# SPECIALIZE withPostgresqlConn :: String -> (Postgresql -> IO a) -> IO a #-}
{-# INLINE withPostgresqlConn #-}
withPostgresqlConn :: (MonadBaseControl IO m, MonadIO m)
               => String
               -> (Postgresql -> m a)
               -> m a
withPostgresqlConn s = bracket (liftIO $ open' s) (liftIO.close')

{-# SPECIALIZE runPostgresqlPool :: DbPersist Postgresql IO a -> Pool Postgresql -> IO a #-}
runPostgresqlPool :: (MonadBaseControl IO m, MonadIO m) => DbPersist Postgresql m a -> Pool Postgresql -> m a
runPostgresqlPool = flip withPool' . runPostgresqlConn

{-# SPECIALIZE runPostgresqlConn :: DbPersist Postgresql IO a -> Postgresql -> IO a #-}
{-# INLINE runPostgresqlConn #-}
runPostgresqlConn :: (MonadBaseControl IO m, MonadIO m) => DbPersist Postgresql m a -> Postgresql -> m a
runPostgresqlConn f conn@(Postgresql c _) = do
  x <- onException (runDbPersist f conn) (liftIO $ H.rollback c)
  liftIO $ H.commit c
  return x

open' :: String -> IO Postgresql
open' s = do
  conn <- H.connectPostgreSQL s
  stmt <- H.prepare conn "SET client_min_messages TO WARNING"
  H.execute stmt []
  cache <- newIORef Map.empty
  return $ Postgresql conn cache

close' :: Postgresql -> IO ()
close' (Postgresql conn _) = H.disconnect conn

{-# SPECIALIZE insert' :: PersistEntity v => v -> DbPersist Postgresql IO (Key v) #-}
{-# INLINE insert' #-}
insert' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> DbPersist Postgresql m (Key v)
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
      queryRawCached' query (tail vals) $ getKey >=> return . Key
    else do
      let constr = constructors e !! constructorNum
      let cName = name ++ [defDelim] ++ constrName constr
      let query = "INSERT INTO " ++ escape name ++ "(discr$)VALUES(?)RETURNING(id$)"
      rowid <- queryRawCached' query (take 1 vals) getKey
      let cQuery = insertIntoConstructorTable True cName constr
      executeRaw True cQuery $ PersistInt64 rowid:(tail vals)
      return $ Key rowid

insertIntoConstructorTable :: Bool -> String -> ConstructorDef -> String
insertIntoConstructorTable withId tName c = "INSERT INTO " ++ escape tName ++ "(" ++ fieldNames ++ ")VALUES(" ++ placeholders ++ ")RETURNING(id$)" where
  fields = if withId then (constrId, namedType (0 :: Int64)):constrParams c else constrParams c
  fieldNames   = fromStringS (renderFields escapeS fields) ""
  placeholders = fromStringS (renderFields (const $ fromChar '?') fields) ""

{-# SPECIALIZE insertBy' :: PersistEntity v => v -> DbPersist Postgresql IO (Either (Key v) (Key v)) #-}
insertBy' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Postgresql m (Either (Key v) (Key v))
insertBy' v = do
  let e = entityDef v
  let name = getEntityName e

  let (constructorNum, constraints) = getConstraints v
  let constrDefs = constrConstrs $ constructors e !! constructorNum
  let constrCond = intercalate " OR " $ map (intercalate " AND " . map (\fname -> escape fname ++ "=?")) $ map (\(Constraint _ fields) -> fields) constrDefs

  let ifAbsent tname ins = if null constraints
       then liftM (Right . Key) ins
       else do
         let query = "SELECT " ++ constrId ++ " FROM " ++ escape tname ++ " WHERE " ++ constrCond
         x <- queryRawCached' query (concatMap snd constraints) firstRow
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
        queryRawCached' query (tail vals) getKey
    else do
      let constr = constructors e !! constructorNum
      let cName = name ++ [defDelim] ++ constrName constr
      ifAbsent cName $ do
        let query = "INSERT INTO " ++ escape name ++ "(discr$)VALUES(?)RETURNING(id$)"
        vals <- toEntityPersistValues v
        rowid <- queryRawCached' query (take 1 vals) getKey
        let cQuery = insertIntoConstructorTable True cName constr
        executeRaw True cQuery $ PersistInt64 rowid :(tail vals)
        return rowid

replace' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => Key v -> v -> DbPersist Postgresql m ()
replace' k v = do
  vals <- toEntityPersistValues v
  let e = entityDef v
  let name = getEntityName e
  let constructorNum = fromPrim (head vals)
  let constr = constructors e !! constructorNum

  let upds = fromStringS (renderFields (\f -> f <> "=?") $ constrParams constr) ""
  let mkQuery tname = "UPDATE " ++ escape tname ++ " SET " ++ upds ++ " WHERE " ++ constrId ++ "=?"

  if isSimple (constructors e)
    then executeRaw True (mkQuery name) (tail vals ++ [toPrim k])
    else do
      let query = "SELECT discr$ FROM " ++ escape name ++ " WHERE id$=?"
      x <- queryRawCached' query [toPrim k] (firstRow >=> return.fmap (fromPrim . head))
      case x of
        Just discr -> do
          let cName = name ++ [defDelim] ++ constrName constr

          if discr == constructorNum
            then executeRaw True (mkQuery cName) (tail vals ++ [toPrim k])
            else do
              let insQuery = insertIntoConstructorTable True cName constr
              executeRaw True insQuery (toPrim k:tail vals)

              let oldCName = name ++ [defDelim] ++ constrName (constructors e !! discr)
              let delQuery = "DELETE FROM " ++ escape oldCName ++ " WHERE " ++ constrId ++ "=?"
              executeRaw True delQuery [toPrim k]

              let updateDiscrQuery = "UPDATE " ++ escape name ++ " SET discr$=? WHERE id$=?"
              executeRaw True updateDiscrQuery [head vals, toPrim k]
        Nothing -> return ()

-- | receives constructor number and row of values from the constructor table
mkEntity :: (PersistEntity v, PersistBackend m) => Int -> [PersistValue] -> m (Key v, v)
mkEntity i (k:xs) = fromEntityPersistValues (toPrim i:xs) >>= \v -> return (fromPrim k, v)
mkEntity _ [] = error "Unable to create entity. No values supplied"

select' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => Cond v c -> [Order v c] -> Int -> Int -> DbPersist Postgresql m [(Key v, v)]
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
        (0, o) -> (" OFFSET ?", [toPrim o])
        (l, 0) -> (" LIMIT ?", [toPrim l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrim l, toPrim o])
  cond' = renderCond' cond
  fields = fromString constrId <> fromChar ',' <> renderFields escapeS (constrParams constr)
  mkQuery tname = fromStringS ("SELECT " <> fields <> " FROM " <> fromString (escape tname) <> whereClause <> orders <> lim) ""
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  doSelectQuery query cNum = queryRawCached' query binds $ mapAllRows (mkEntity cNum)
  binds = maybe id getValues cond' $ limps
  constr = constructors e !! phantomConstrNum (undefined :: c)

selectAll' :: forall m v.(MonadBaseControl IO m, MonadIO m, PersistEntity v) => DbPersist Postgresql m [(Key v, v)]
selectAll' = start where
  start = if isSimple (constructors e)
    then let
      constr = head $ constructors e
      fields = fromString constrId <> fromChar ',' <> renderFields escapeS (constrParams constr)
      query = "SELECT " ++ fromStringS fields (" FROM " ++ escape name)
      in queryRawCached' query [] $ mapAllRows (mkEntity 0)
    else liftM concat $ forM (zip [0..] (constructors e)) $ \(i, constr) -> do
        let fields = fromString constrId <> fromChar ',' <> renderFields escapeS (constrParams constr)
        let cName = name ++ [defDelim] ++ constrName constr
        let query = "SELECT " ++ fromStringS fields (" FROM " ++ escape cName)
        queryRawCached' query [] $ mapAllRows (mkEntity i)
  e = entityDef (undefined :: v)
  name = getEntityName e

{-# SPECIALIZE get' :: PersistEntity v => Key v -> DbPersist Postgresql IO (Maybe v) #-}
{-# INLINE get' #-}
get' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => Key v -> DbPersist Postgresql m (Maybe v)
get' (k :: Key v) = do
  let e = entityDef (undefined :: v)
  let name = getEntityName e
  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let fields = fromString constrId <> fromChar ',' <> renderFields escapeS (constrParams constr)
      let query = fromStringS ("SELECT " <> fields) $ " FROM " ++ escape name ++ " WHERE " ++ constrId ++ "=?"
      x <- queryRawCached' query [toPrim k] firstRow
      case x of
        Just (_:xs) -> liftM Just $ fromEntityPersistValues $ PersistInt64 0:xs
        Just x'    -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing
    else do
      let query = "SELECT discr$ FROM " ++ escape name ++ " WHERE id$=?"
      x <- queryRawCached' query [toPrim k] firstRow
      case x of
        Just [discr] -> do
          let constructorNum = fromPrim discr
          let constr = constructors e !! constructorNum
          let cName = name ++ [defDelim] ++ constrName constr
          let fields = fromString constrId <> fromChar ',' <> renderFields escapeS (constrParams constr)
          let cQuery = fromStringS ("SELECT " <> fields) $ " FROM " ++ escape cName ++ " WHERE " ++ constrId ++ "=?"
          x2 <- queryRawCached' cQuery [toPrim k] firstRow
          case x2 of
            Just (_:xs) -> liftM Just $ fromEntityPersistValues $ discr:xs
            Just x2'    -> fail $ "Unexpected number of columns returned: " ++ show x2'
            Nothing     -> fail "Missing entry in constructor table"
        Just x' -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing

update' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => [Update v c] -> Cond v c -> DbPersist Postgresql m ()
update' upds (cond :: Cond v c) = do
  let e = entityDef (undefined :: v)
  let name = getEntityName e
  case renderUpdates escapeS upds of
    Just upds' -> do
      let conds' = renderCond' cond
      let mkQuery tname = "UPDATE " ++ escape tname ++ " SET " ++ fromStringS whereClause "" where
          whereClause = maybe (getQuery upds') (\c -> getQuery upds' <> " WHERE " <> getQuery c) conds'
      let qName = if isSimple (constructors e) then name else name ++ [defDelim] ++ phantomConstrName (undefined :: c)
      executeRawCached' (mkQuery qName) (getValues upds' <> maybe mempty getValues conds' $ [])
    Nothing -> return ()

delete' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => Cond v c -> DbPersist Postgresql m ()
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

deleteByKey' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => Key v -> DbPersist Postgresql m ()
deleteByKey' (k :: Key v) = do
  let e = entityDef (undefined :: v)
  let name = getEntityName e
  let query = "DELETE FROM " ++ escape name ++ " WHERE id$=?"
  executeRawCached' query [toPrim k]

{-# SPECIALIZE count' :: (PersistEntity v, Constructor c) => Cond v c -> DbPersist Postgresql IO Int #-}
count' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => Cond v c -> DbPersist Postgresql m Int
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

{-# SPECIALIZE countAll' :: PersistEntity v => v -> DbPersist Postgresql IO Int #-}
countAll' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Postgresql m Int
countAll' (_ :: v) = do
  let name = persistName (undefined :: v)
  let query = "SELECT COUNT(*) FROM " ++ escape name
  x <- queryRawCached' query [] firstRow
  case x of
    Just [num] -> return $ fromPrim num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"
    
insertList' :: forall m a.(MonadBaseControl IO m, MonadIO m, PersistField a) => [a] -> DbPersist Postgresql m Int64
insertList' (l :: [a]) = do
  let mainName = "List$$" ++ persistName (undefined :: a)
  k <- queryRawCached' ("INSERT INTO " ++ escape mainName ++ " DEFAULT VALUES RETURNING(id$)") [] getKey
  let valuesName = mainName ++ "$" ++ "values"
  let fields = [("ord$", namedType (0 :: Int)), ("value", namedType (undefined :: a))]
  let query = "INSERT INTO " ++ escape valuesName ++ "(id$," ++ fromStringS (renderFields escapeS fields <> ")VALUES(?," <> renderFields (const $ fromChar '?') fields) ")"
  let go :: Int -> [a] -> DbPersist Postgresql m ()
      go n (x:xs) = do
       x' <- toPersistValues x
       executeRaw True query $ (toPrim k:) . (toPrim n:) . x' $ []
       go (n + 1) xs
      go _ [] = return ()
  go 0 l
  return k
  
getList' :: forall m a.(MonadBaseControl IO m, MonadIO m, PersistField a) => Int64 -> DbPersist Postgresql m [a]
getList' k = do
  let mainName = "List$$" ++ persistName (undefined :: a)
  let valuesName = mainName ++ "$values"
  let value = ("value", namedType (undefined :: a))
  let query = fromStringS ("SELECT " <> renderFields escapeS [value] <> " FROM " <> escapeS (fromString valuesName)) " WHERE id$=? ORDER BY ord$"
  queryRawCached' query [toPrim k] $ mapAllRows (liftM fst . fromPersistValues)

{-# SPECIALIZE getKey :: RowPopper (DbPersist Postgresql IO) -> DbPersist Postgresql IO Int64 #-}
getKey :: MonadIO m => RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m Int64
getKey pop = pop >>= \(Just [k]) -> return $ fromPrim k

----------

getStatementCached :: MonadIO m => String -> DbPersist Postgresql m H.Statement
getStatementCached sql = do
  Postgresql conn smap <- DbPersist ask
  liftIO $ do
    smap' <- readIORef smap
    case Map.lookup sql smap' of
      Nothing -> do
        stmt <- H.prepare conn sql
        writeIORef smap (Map.insert sql stmt smap')
        return stmt
      Just stmt -> return stmt

executeRaw' :: MonadIO m => String -> [PersistValue] -> DbPersist Postgresql m ()
executeRaw' query vals = do
  stmt <- getStatement query
  liftIO $ do
    H.execute stmt (map pToSql vals)
    return ()

{-# SPECIALIZE executeRawCached' :: String -> [PersistValue] -> DbPersist Postgresql IO () #-}
executeRawCached' :: MonadIO m => String -> [PersistValue] -> DbPersist Postgresql m ()
executeRawCached' query vals = do
  stmt <- getStatementCached query
  liftIO $ do
    H.execute stmt (map pToSql vals)
    return ()

queryRawCached' :: (MonadBaseControl IO m, MonadIO m) => String -> [PersistValue] -> (RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m a) -> DbPersist Postgresql m a
queryRawCached' query vals f = do
  stmt <- getStatementCached query
  liftIO $ H.execute stmt (map pToSql vals)
  f $ liftIO $ do
    x <- H.fetchRow stmt
    return $ fmap (map pFromSql) x

renderCond' :: (PersistEntity v, Constructor c) => Cond v c -> Maybe (RenderS StringS)
renderCond' = renderCond escapeS constrId renderEquals renderNotEquals where
  renderEquals a b = a <> " IS NOT DISTINCT FROM " <> b
  renderNotEquals a b = a <> " IS DISTINCT FROM " <> b

escapeS :: StringS -> StringS
escapeS a = let q = fromChar '"' in q <> a <> q
