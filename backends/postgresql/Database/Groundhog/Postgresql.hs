{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, OverloadedStrings #-}
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
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql.String
import Database.Groundhog.Postgresql.Base
import Database.Groundhog.Postgresql.Migration

import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as H

import Control.Exception.Control (bracket, onException)
import Control.Monad(liftM, forM, (>=>))
import Control.Monad.IO.Control (MonadControlIO)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class(MonadTrans(..))
import Control.Monad.Trans.Reader(ask)
import Data.Enumerator(Enumerator, Iteratee(..), Stream(..), checkContinue0, (>>==), joinE, runIteratee, continue, concatEnums)
import qualified Data.Enumerator.List as EL
import Data.Int (Int64)
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as Map
import Data.Pool

instance MonadControlIO m => PersistBackend (DbPersist Postgresql m) where
  {-# SPECIALIZE instance PersistBackend (DbPersist Postgresql IO) #-}
  insert v = insert' v
  insertBy v = insertBy' v
  replace k v = replace' k v
  selectEnum cond ords limit offset = selectEnum' cond ords limit offset
  selectAllEnum = selectAllEnum'
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
  insertTuple t ts = insertTuple' t ts
  getTuple t k = getTuple' t k

--{-# SPECIALIZE withPostgresqlPool :: String -> Int -> (Pool Postgresql -> IO a) -> IO a #-}
withPostgresqlPool :: MonadControlIO m
               => String
               -> Int -- ^ number of connections to open
               -> (Pool Postgresql -> m a)
               -> m a
withPostgresqlPool s = createPool (open' s) close'

{-# SPECIALIZE withPostgresqlConn :: String -> (Postgresql -> IO a) -> IO a #-}
{-# INLINE withPostgresqlConn #-}
withPostgresqlConn :: MonadControlIO m
               => String
               -> (Postgresql -> m a)
               -> m a
withPostgresqlConn s = bracket (liftIO $ open' s) (liftIO.close')

{-# SPECIALIZE runPostgresqlPool :: DbPersist Postgresql IO a -> Pool Postgresql -> IO a #-}
runPostgresqlPool :: MonadControlIO m => DbPersist Postgresql m a -> Pool Postgresql -> m a
runPostgresqlPool = flip withPool' . runPostgresqlConn

{-# SPECIALIZE runPostgresqlConn :: DbPersist Postgresql IO a -> Postgresql -> IO a #-}
{-# INLINE runPostgresqlConn #-}
runPostgresqlConn :: MonadControlIO m => DbPersist Postgresql m a -> Postgresql -> m a
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

{-
DbMaybe prim -> name type
prim         -> name type NOT NULL
comp         -> name type NOT NULL REFERENCES table
DbMaybe comp -> name type REFERENCES table
-}

{-# SPECIALIZE insert' :: PersistEntity v => v -> DbPersist Postgresql IO (Key v) #-}
{-# INLINE insert' #-}
insert' :: (PersistEntity v, MonadControlIO m) => v -> DbPersist Postgresql m (Key v)
insert' v = do
  -- constructor number and the rest of the field values
  vals <- toPersistValues v
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

-- in Sqlite we can insert null to the id column. If so, id will be generated automatically
insertIntoConstructorTable :: Bool -> String -> ConstructorDef -> String
insertIntoConstructorTable withId tName c = "INSERT INTO " ++ escape tName ++ "(" ++ fieldNames ++ ")VALUES(" ++ placeholders ++ ")RETURNING(id$)" where
  fieldNames   = intercalate "," $ (if withId then (constrId:) else id) $ map (escape.fst) (constrParams c)
  placeholders = intercalate "," $ (if withId then ("?":) else id) $ map (const "?") (constrParams c)

{-# SPECIALIZE insertBy' :: PersistEntity v => v -> DbPersist Postgresql IO (Either (Key v) (Key v)) #-}
insertBy' :: (MonadControlIO m, PersistEntity v) => v -> DbPersist Postgresql m (Either (Key v) (Key v))
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
         x <- queryRawCached' query (concatMap (map snd) constraintFields) firstRow
         case x of
           Nothing  -> liftM (Right . Key) ins
           Just [k] -> return $ Left $ fromPrim k
           Just xs  -> fail $ "unexpected query result: " ++ show xs

  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      ifAbsent name $ do
        let query = insertIntoConstructorTable False name constr
        vals <- toPersistValues v
        queryRawCached' query (tail vals) getKey
    else do
      let constr = constructors e !! constructorNum
      let cName = name ++ [defDelim] ++ constrName constr
      ifAbsent cName $ do
        let query = "INSERT INTO " ++ escape name ++ "(discr$)VALUES(?)RETURNING(id$)"
        vals <- toPersistValues v
        rowid <- queryRawCached' query (take 1 vals) getKey
        let cQuery = insertIntoConstructorTable True cName constr
        executeRaw True cQuery $ PersistInt64 rowid :(tail vals)
        return rowid

replace' :: (MonadControlIO m, PersistEntity v) => Key v -> v -> DbPersist Postgresql m ()
replace' k v = do
  vals <- toPersistValues v
  let e = entityDef v
  let name = getEntityName e
  let constructorNum = fromPrim (head vals)
  let constr = constructors e !! constructorNum

  let upds = intercalate "," $ map (\f -> escape (fst f) ++ "=?") $ constrParams constr
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

              -- UGLY: reinsert entry with a new discr to the main table after it was deleted by a trigger.
              let reInsQuery = "INSERT INTO " ++ escape name ++ "(id$,discr$)VALUES(?,?)"
              executeRaw True reInsQuery [toPrim k, head vals]
        Nothing -> return ()

-- | receives constructor number and row of values from the constructor table
mkEntity :: (PersistEntity v, PersistBackend m) => Int -> [PersistValue] -> m (Key v, v)
mkEntity i (k:xs) = fromPersistValues (toPrim i:xs) >>= \v -> return (fromPrim k, v)
mkEntity _ [] = error "Unable to create entity. No values supplied"

selectEnum' :: (MonadControlIO m, PersistEntity v, Constructor c) => Cond v c -> [Order v c] -> Int -> Int -> Enumerator (Key v, v) (DbPersist Postgresql m) a
selectEnum' (cond :: Cond v c) ords limit offset = start where
  start = if isSimple (constructors e)
    then joinE (queryEnum (mkQuery name) binds) (EL.mapM (mkEntity 0))
    else let
      query = mkQuery $ name ++ [defDelim] ++ constrName constr
      in joinE (queryEnum query binds) (EL.mapM (mkEntity $ constrNum constr))

  e = entityDef (undefined :: v)
  orders = renderOrders escape ords
  name = getEntityName e
  (lim, limps) = case (limit, offset) of
        (0, 0) -> ("", [])
        (0, o) -> (" LIMIT -1 OFFSET ?", [toPrim o])
        (l, 0) -> (" LIMIT ?", [toPrim l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrim l, toPrim o])
  conds' = renderCond' cond
  mkQuery tname = "SELECT * FROM " ++ escape tname ++ " WHERE " ++ fromStringS (getQuery conds' <> orders <> lim)
  binds = getValues conds' limps
  constr = constructors e !! phantomConstrNum (undefined :: c)

selectAllEnum' :: forall m v a.(MonadControlIO m, PersistEntity v) => Enumerator (Key v, v) (DbPersist Postgresql m) a
selectAllEnum' = start where
  start = if isSimple (constructors e)
    then let
      query = "SELECT * FROM " ++ escape name
      in joinE (queryEnum query []) (EL.mapM (mkEntity 0))
    else concatEnums $ zipWith q [0..] (constructors e) where
      q cNum constr = let
        cName = name ++ [defDelim] ++ constrName constr
        query = "SELECT * FROM " ++ escape cName
        in joinE (queryEnum query []) (EL.mapM (mkEntity cNum))

  e = entityDef (undefined :: v)
  name = getEntityName e

-- unfortunately, running consume on Enumerator is ~50% slower. So, lets duplicate the code
select' :: (MonadControlIO m, PersistEntity v, Constructor c) => Cond v c -> [Order v c] -> Int -> Int -> DbPersist Postgresql m [(Key v, v)]
select' (cond :: Cond v c) ords limit offset = start where
  start = if isSimple (constructors e)
    then doSelectQuery (mkQuery name) 0
    else let
      cName = name ++ [defDelim] ++ constrName constr
      in doSelectQuery (mkQuery cName) $ constrNum constr

  e = entityDef (undefined :: v)
  orders = renderOrders escape ords
  name = getEntityName e
  (lim, limps) = case (limit, offset) of
        (0, 0) -> ("", [])
        (0, o) -> (" LIMIT -1 OFFSET ?", [toPrim o])
        (l, 0) -> (" LIMIT ?", [toPrim l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrim l, toPrim o])
  conds' = renderCond' cond
  mkQuery tname = "SELECT * FROM " ++ escape tname ++ " WHERE " ++ fromStringS (getQuery conds' <> orders <> lim)
  doSelectQuery query cNum = queryRawCached' query binds $ mapAllRows (mkEntity cNum)
  binds = getValues conds' limps
  constr = constructors e !! phantomConstrNum (undefined :: c)

selectAll' :: forall m v.(MonadControlIO m, PersistEntity v) => DbPersist Postgresql m [(Key v, v)]
selectAll' = start where
  start = if isSimple (constructors e)
    then let
      query = "SELECT * FROM " ++ escape name
      in queryRawCached' query [] $ mapAllRows (mkEntity 0)
    else liftM concat $ forM (zip [0..] (constructors e)) $ \(i, constr) -> do
        let cName = name ++ [defDelim] ++ constrName constr
        let query = "SELECT * FROM " ++ escape cName
        queryRawCached' query [] $ mapAllRows (mkEntity i)

  e = entityDef (undefined :: v)
  name = getEntityName e

{-
insertList :: PersistField a => [a] -> DbPersist conn m Int64
insertList xs = do
  xs' <- mapM toPersistValue xs
  let name = persistName xs
  let query = "INSERT INTO " ++ name ++ " ("
  getStatement 
-}

insertTuple' :: MonadControlIO m => NamedType -> [PersistValue] -> DbPersist Postgresql m Int64
insertTuple' t vals = do
  let name = getName t
  let (DbTuple _ ts) = getType t
  let fields = map (\i -> "val" ++ show i) [0 .. length ts - 1] 
  let query = "INSERT INTO " ++ escape name ++ " (" ++ intercalate ", " fields ++ ")VALUES(" ++ intercalate ", " (replicate (length ts) "?") ++ ")RETURNING(id$)"
  queryRawCached' query vals getKey

getTuple' :: MonadControlIO m => NamedType -> Int64 -> DbPersist Postgresql m [PersistValue]
getTuple' t k = do
  let name = getName t
  let query = "SELECT * FROM " ++ escape name ++ " WHERE id$=?"
  x <- queryRawCached' query [toPrim k] firstRow
  maybe (fail $ "No tuple with id " ++ show k) (return . tail) x

{-# SPECIALIZE get' :: PersistEntity v => Key v -> DbPersist Postgresql IO (Maybe v) #-}
{-# INLINE get' #-}
get' :: (MonadControlIO m, PersistEntity v) => Key v -> DbPersist Postgresql m (Maybe v)
get' (k :: Key v) = do
  let e = entityDef (undefined :: v)
  let name = getEntityName e
  if isSimple (constructors e)
    then do
      let query = "SELECT * FROM " ++ escape name ++ " WHERE " ++ constrId ++ "=?"
      x <- queryRawCached' query [toPrim k] firstRow
      case x of
        Just (_:xs) -> liftM Just $ fromPersistValues $ PersistInt64 0:xs
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
          let cQuery = "SELECT * FROM " ++ escape cName ++ " WHERE " ++ constrId ++ "=?"
          x2 <- queryRawCached' cQuery [toPrim k] firstRow
          case x2 of
            Just (_:xs) -> liftM Just $ fromPersistValues $ discr:xs
            Just x2'    -> fail $ "Unexpected number of columns returned: " ++ show x2'
            Nothing     -> fail "Missing entry in constructor table"
        Just x' -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing

update' :: (MonadControlIO m, PersistEntity v, Constructor c) => [Update v c] -> Cond v c -> DbPersist Postgresql m ()
update' upds (cond :: Cond v c) = do
  let e = entityDef (undefined :: v)
  let name = getEntityName e
  let conds' = renderCond' cond
  let upds' = renderUpdates escape upds
  let mkQuery tname = "UPDATE " ++ escape tname ++ " SET " ++ fromStringS (getQuery upds' <> " WHERE " <> getQuery conds')
  let qName = if isSimple (constructors e) then name else name ++ [defDelim] ++ phantomConstrName (undefined :: c)
  executeRawCached' (mkQuery qName) (getValues upds' <> getValues conds' $ [])

delete' :: (MonadControlIO m, PersistEntity v, Constructor c) => Cond v c -> DbPersist Postgresql m ()
delete' (cond :: Cond v c) = do
  let e = entityDef (undefined :: v)
  let cond' = renderCond' cond
  let name = getEntityName e
  if isSimple (constructors e)
    then do
      let query = "DELETE FROM " ++ escape name ++ " WHERE " ++ fromStringS (getQuery cond')
      executeRawCached' query (getValues cond' [])
    else do
      let cName = name ++ [defDelim] ++ phantomConstrName (undefined :: c)
      let query = "DELETE FROM " ++ escape name ++ " WHERE id$ IN(SELECT id$ FROM " ++ escape cName ++ " WHERE " ++ fromStringS (getQuery cond') ++ ")"
      -- the entries in the constructor table are deleted because of the reference on delete cascade
      executeRawCached' query (getValues cond' [])

deleteByKey' :: (MonadControlIO m, PersistEntity v) => Key v -> DbPersist Postgresql m ()
deleteByKey' (k :: Key v) = do
  let e = entityDef (undefined :: v)
  let name = getEntityName e
  let query = "DELETE FROM " ++ escape name ++ " WHERE id$=?"
  executeRawCached' query [toPrim k]

{-# SPECIALIZE count' :: (PersistEntity v, Constructor c) => Cond v c -> DbPersist Postgresql IO Int #-}
count' :: (MonadControlIO m, PersistEntity v, Constructor c) => Cond v c -> DbPersist Postgresql m Int
count' (cond :: Cond v c) = do
  let cName = persistName (undefined :: v) ++ [defDelim] ++ phantomConstrName (undefined :: c)
  let cond' = renderCond' cond
  let query = "SELECT COUNT(*) FROM " ++ escape cName ++ " WHERE " ++ fromStringS (getQuery cond')
  x <- queryRawCached' query (getValues cond' []) firstRow
  case x of
    Just [num] -> return $ fromPrim num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

{-# SPECIALIZE countAll' :: PersistEntity v => v -> DbPersist Postgresql IO Int #-}
countAll' :: (MonadControlIO m, PersistEntity v) => v -> DbPersist Postgresql m Int
countAll' (_ :: v) = do
  let name = persistName (undefined :: v)
  let query = "SELECT COUNT(*) FROM " ++ escape name
  x <- queryRawCached' query [] firstRow
  case x of
    Just [num] -> return $ fromPrim num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"
    
insertList' :: forall m a.(MonadControlIO m, PersistField a) => [a] -> DbPersist Postgresql m Int64
insertList' l = do
  let mainName = "List$$" ++ persistName (undefined :: a)
  k <- queryRawCached' ("INSERT INTO " ++ escape mainName ++ " DEFAULT VALUES RETURNING(id$)") [] getKey
  let valuesName = mainName ++ "$" ++ "values"
  let query = "INSERT INTO " ++ escape valuesName ++ "(id$,ord$,value)VALUES(?,?,?)"
  let go :: Int -> [a] -> DbPersist Postgresql m ()
      go n (x:xs) = do
       x' <- toPersistValue x
       executeRaw True query [toPrim k, toPrim n, x']
       go (n + 1) xs
      go _ [] = return ()
  go 0 l
  return k
  
getList' :: forall m a.(MonadControlIO m, PersistField a) => Int64 -> DbPersist Postgresql m [a]
getList' k = do
  let mainName = "List$$" ++ persistName (undefined :: a)
  let valuesName = mainName ++ "$values"
  queryRawCached' ("SELECT value FROM " ++ escape valuesName ++ " WHERE id$=? ORDER BY ord$") [toPrim k] $ mapAllRows (fromPersistValue.head)

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

queryRawCached' :: MonadControlIO m => String -> [PersistValue] -> (RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m a) -> DbPersist Postgresql m a
queryRawCached' query vals f = do
  stmt <- getStatementCached query
  liftIO $ H.execute stmt (map pToSql vals)
  f $ liftIO $ do
    x <- H.fetchRow stmt
    return $ fmap (map pFromSql) x

queryEnum :: MonadControlIO m => String -> [PersistValue] -> Enumerator [PersistValue] (DbPersist Postgresql m) b
queryEnum query vals = \step -> do
  stmt <- lift $ getStatementCached query
  liftIO $ H.execute stmt (map pToSql vals)
  let iter = checkContinue0 $ \loop k -> do
      x <- liftIO $ do
        x <- H.fetchRow stmt
        return $ fmap (map pFromSql) x
      maybe (continue k) (\row -> k (Chunks [row]) >>== loop) x
  Iteratee (runIteratee (iter step))

renderCond' :: (PersistEntity v, Constructor c) => Cond v c -> RenderS StringS
renderCond' = renderCond escape constrId renderEquals renderNotEquals where
  renderEquals :: (String -> String) -> Expr v c a -> Expr v c a -> RenderS StringS
  renderEquals esc a b = renderExpr esc a <> RenderS " IS NOT DISTINCT FROM " id <> renderExpr esc b

  renderNotEquals :: (String -> String) -> Expr v c a -> Expr v c a -> RenderS StringS
  renderNotEquals esc a b = renderExpr esc a <> RenderS " IS DISTINCT FROM " id <> renderExpr esc b
