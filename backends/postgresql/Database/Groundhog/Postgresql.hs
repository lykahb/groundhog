{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies #-}
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

import qualified Database.PostgreSQL.Simple as PG

import Control.Monad (liftM, forM, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control(MonadBaseControl)
import Control.Monad.Trans.Reader (ask)
import Data.ByteString.Char8 (pack)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (fromJust) 
import Data.Monoid
import Data.Conduit.Pool

instance (MonadBaseControl IO m, MonadIO m) => PersistBackend (DbPersist Postgresql m) where
  {-# SPECIALIZE instance PersistBackend (DbPersist Postgresql IO) #-}
  type PhantomDb (DbPersist Postgresql m) = Postgresql
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

--{-# SPECIALIZE withPostgresqlPool :: String -> Int -> (Pool Postgresql -> IO a) -> IO a #-}
withPostgresqlPool :: (MonadBaseControl IO m, MonadIO m)
               => String
               -> Int -- ^ number of connections to open
               -> (Pool Postgresql -> m a)
               -> m a
withPostgresqlPool s connCount f = liftIO (createPool (open' s) close' 1 20 connCount) >>= f

{-# SPECIALIZE withPostgresqlConn :: String -> (Postgresql -> IO a) -> IO a #-}
{-# INLINE withPostgresqlConn #-}
withPostgresqlConn :: (MonadBaseControl IO m, MonadIO m)
               => String
               -> (Postgresql -> m a)
               -> m a
withPostgresqlConn s = bracket (liftIO $ open' s) (liftIO . close')

{-# SPECIALIZE runPostgresqlPool :: DbPersist Postgresql IO a -> Pool Postgresql -> IO a #-}
runPostgresqlPool :: (MonadBaseControl IO m, MonadIO m) => DbPersist Postgresql m a -> Pool Postgresql -> m a
runPostgresqlPool f pconn = withResource pconn $ runPostgresqlConn f

{-# SPECIALIZE runPostgresqlConn :: DbPersist Postgresql IO a -> Postgresql -> IO a #-}
{-# INLINE runPostgresqlConn #-}
runPostgresqlConn :: (MonadBaseControl IO m, MonadIO m) => DbPersist Postgresql m a -> Postgresql -> m a
runPostgresqlConn f conn@(Postgresql c) = do
  liftIO $ PG.begin c
  x <- onException (runDbPersist f conn) (liftIO $ PG.rollback c)
  liftIO $ PG.commit c
  return x

open' :: String -> IO Postgresql
open' s = do
  conn <- PG.connectPostgreSQL $ pack s
  PG.execute_ conn $ getStatement "SET client_min_messages TO WARNING"
  return $ Postgresql conn

close' :: Postgresql -> IO ()
close' (Postgresql conn) = PG.close conn

{-# SPECIALIZE insert' :: PersistEntity v => v -> DbPersist Postgresql IO (AutoKey v) #-}
{-# INLINE insert' #-}
insert' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> DbPersist Postgresql m (AutoKey v)
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
      case constrAutoKeyName constr of
        Nothing -> executeRawCached' query (tail vals) >> pureFromPersistValue []
        Just _  -> do
          x <- queryRawCached' query (tail vals) id
          case x of
            Just xs -> pureFromPersistValue xs
            Nothing -> pureFromPersistValue []
    else do
      let constr = constructors e !! constructorNum
      let cName = name ++ [delim] ++ constrName constr
      let query = "INSERT INTO " <> escapeS (fromString name) <> "(discr)VALUES(?)RETURNING(id)"
      rowid <- queryRawCached' query (take 1 vals) getKey
      let cQuery = insertIntoConstructorTable True cName constr
      executeRawCached' cQuery $ rowid:(tail vals)
      pureFromPersistValue [rowid]

insertBy' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
          => u (UniqueMarker v) -> v -> DbPersist Postgresql m (Either (AutoKey v) (AutoKey v))
insertBy' u v = do
  let e = entityDef v
  let name = persistName v
  uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
  let fields = foldr (renderChain escapeS) [] $ projectionFieldChains u []
  let cond = intercalateS " AND " $ map (<> "=?") fields
  
  let ifAbsent tname constr = do
      let query = "SELECT " <> maybe "1" id (constrId constr) <> " FROM " <> escapeS (fromString tname) <> " WHERE " <> cond
      x <- queryRawCached' query (uniques []) id
      case x of
        Nothing  -> liftM Right $ insert v
        Just [k] -> return $ Left $ fst $ fromPurePersistValues proxy [k]
        Just xs  -> fail $ "unexpected query result: " ++ show xs
  let constr = head $ constructors e
  ifAbsent name constr

insertIntoConstructorTable :: Bool -> String -> ConstructorDef -> StringS
insertIntoConstructorTable withId tName c = "INSERT INTO " <> escapeS (fromString tName) <> "(" <> fieldNames <> ")VALUES(" <> placeholders <> ")" <> returning where
  (fields, returning) = case constrAutoKeyName c of
    Just idName | withId    -> ((idName, dbType (0 :: Int64)):constrParams c, mempty)
                | otherwise -> (constrParams c, "RETURNING(" <> escapeS (fromString idName) <> ")")
    _                       -> (constrParams c, mempty)
  fieldNames   = renderFields escapeS fields
  placeholders = renderFields (const $ fromChar '?') fields

{-# SPECIALIZE insertByAll' :: PersistEntity v => v -> DbPersist Postgresql IO (Either (AutoKey v) (AutoKey v)) #-}
insertByAll' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Postgresql m (Either (AutoKey v) (AutoKey v))
insertByAll' v = do
  let e = entityDef v
  let name = persistName v

  let (constructorNum, uniques) = getUniques proxy v
  let uniqueDefs = constrUniques $ constructors e !! constructorNum
  let cond = fromString $ intercalate " OR " $ map (intercalate " AND " . map (\(fname, _) -> escape fname ++ "=?")) $ map (\(UniqueDef _ fields) -> fields) uniqueDefs

  let ifAbsent tname constr = do
      let query = "SELECT " <> maybe "1" id (constrId constr) <> " FROM " <> escapeS (fromString tname) <> " WHERE " <> cond
      x <- queryRawCached' query (concatMap snd uniques) id
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
         => Key v BackendSpecific -> v -> DbPersist Postgresql m ()
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
      x <- queryRawCached' query [toPrimitivePersistValue proxy k] (id >=> return . fmap (fromPrimitivePersistValue proxy . head))
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

-- | receives constructor number and row of values from the constructor table
mkEntity :: (PersistEntity v, PersistBackend m) => Int -> [PersistValue] -> m (AutoKey v, v)
mkEntity i xs = fromEntityPersistValues (toPrimitivePersistValue proxy i:xs') >>= \(v, _) -> return (k, v) where
  (k, xs') = fromPurePersistValues proxy xs

select' :: forall m v c opts . (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c, HasSelectOptions opts v c)
        => opts -> DbPersist Postgresql m [v]
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
        (Nothing, o) -> (" OFFSET ?", [toPrimitivePersistValue proxy o])
        (l, Nothing) -> (" LIMIT ?", [toPrimitivePersistValue proxy l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrimitivePersistValue proxy l, toPrimitivePersistValue proxy o])
  cond' = renderCond' cond
  fields = renderFields escapeS (constrParams constr)
  mkQuery tname = "SELECT " <> fields <> " FROM " <> fromString (escape tname) <> whereClause <> orders <> lim
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  doSelectQuery query cNum = queryRawCached' query binds $ mapAllRows $ liftM fst . fromEntityPersistValues . (toPrimitivePersistValue proxy cNum:)
  binds = maybe id getValues cond' $ limps
  constr = constructors e !! phantomConstrNum (undefined :: c a)

selectAll' :: forall m v.(MonadBaseControl IO m, MonadIO m, PersistEntity v) => DbPersist Postgresql m [(AutoKey v, v)]
selectAll' = start where
  start = if isSimple (constructors e)
    then let
      constr = head $ constructors e
      fields = maybe id (\key cont -> key <> fromChar ',' <> cont) (constrId constr) $ renderFields escapeS (constrParams constr)
      query = "SELECT " <> fields <> " FROM " <> escapeS (fromString name)
      in queryRawCached' query [] $ mapAllRows (mkEntity 0)
    else liftM concat $ forM (zip [0..] (constructors e)) $ \(i, constr) -> do
        let fields = fromJust (constrId constr) <> fromChar ',' <> renderFields escapeS (constrParams constr)
        let cName = name ++ [delim] ++ constrName constr
        let query = "SELECT " <> fields <> " FROM " <> escapeS (fromString cName)
        queryRawCached' query [] $ mapAllRows (mkEntity i)
  e = entityDef (undefined :: v)
  name = persistName (undefined :: v)

{-# SPECIALIZE get' :: (PersistEntity v, PrimitivePersistField (Key v BackendSpecific)) => Key v BackendSpecific -> DbPersist Postgresql IO (Maybe v) #-}
{-# INLINE get' #-}
get' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
     => Key v BackendSpecific -> DbPersist Postgresql m (Maybe v)
get' (k :: Key v BackendSpecific) = do
  let e = entityDef (undefined :: v)
  let name = persistName (undefined :: v)
  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let fields = renderFields escapeS (constrParams constr)
      let query = "SELECT " <> fields <> " FROM " <> escapeS (fromString name) <> " WHERE " <> fromJust (constrId constr) <> "=?"
      x <- queryRawCached' query [toPrimitivePersistValue proxy k] id
      case x of
        Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
        Nothing -> return Nothing
    else do
      let query = "SELECT discr FROM " <> escapeS (fromString name) <> " WHERE id=?"
      x <- queryRawCached' query [toPrimitivePersistValue proxy k] id
      case x of
        Just [discr] -> do
          let constructorNum = fromPrimitivePersistValue proxy discr
          let constr = constructors e !! constructorNum
          let cName = name ++ [delim] ++ constrName constr
          let fields = renderFields escapeS (constrParams constr)
          let cQuery = "SELECT " <> fields <> " FROM " <> escapeS (fromString cName) <> " WHERE " <> fromJust (constrId constr) <> "=?"
          x2 <- queryRawCached' cQuery [toPrimitivePersistValue proxy k] id
          case x2 of
            Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ discr:xs
            Nothing -> fail "Missing entry in constructor table"
        Just x' -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing

{-# SPECIALIZE getBy' :: (PersistEntity v, IsUniqueKey (Key v (Unique u))) => Key v (Unique u) -> DbPersist Postgresql IO (Maybe v) #-}
getBy' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
       => Key v (Unique u) -> DbPersist Postgresql m (Maybe v)
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
  x <- queryRawCached' query (uniques []) id
  case x of
    Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
    Nothing -> return Nothing

update' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => [Update v c] -> Cond v c -> DbPersist Postgresql m ()
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

delete' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => Cond v c -> DbPersist Postgresql m ()
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
             => Key v BackendSpecific -> DbPersist Postgresql m ()
deleteByKey' k = executeRawCached' query [toPrimitivePersistValue proxy k] where
  e = entityDef ((undefined :: Key v u -> v) k)
  constr = head $ constructors e
  name = fromString (persistName $ (undefined :: Key v u -> v) k)
  query = "DELETE FROM " <> escapeS name <> " WHERE " <> fromJust (constrId constr) <> "=?"

{-# SPECIALIZE count' :: (PersistEntity v, Constructor c) => Cond v c -> DbPersist Postgresql IO Int #-}
count' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c) => Cond v c -> DbPersist Postgresql m Int
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

{-# SPECIALIZE countAll' :: PersistEntity v => v -> DbPersist Postgresql IO Int #-}
countAll' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Postgresql m Int
countAll' (_ :: v) = do
  let name = persistName (undefined :: v)
  let query = "SELECT COUNT(*) FROM " <> escapeS (fromString name)
  x <- queryRawCached' query [] id
  case x of
    Just [num] -> return $ fromPrimitivePersistValue proxy num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

project' :: forall m v c p opts a' . (MonadBaseControl IO m, MonadIO m, PersistEntity v, Constructor c, Projection p (RestrictionHolder v c) a', HasSelectOptions opts v c)
         => p -> opts -> DbPersist Postgresql m [a']
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
        (Nothing, o) -> (" OFFSET ?", [toPrimitivePersistValue proxy o])
        (l, Nothing) -> (" LIMIT ?", [toPrimitivePersistValue proxy l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrimitivePersistValue proxy l, toPrimitivePersistValue proxy o])
  cond' = renderCond' cond
  chains = projectionFieldChains p []
  fields = intercalateS (fromChar ',') $ foldr (renderChain escapeS) [] chains
  mkQuery tname = "SELECT " <> fields <> " FROM " <> escapeS (fromString tname) <> whereClause <> orders <> lim
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  doSelectQuery query = queryRawCached' query binds $ mapAllRows $ liftM fst . projectionResult p
  binds = maybe id getValues cond' $ limps
  constr = constructors e !! phantomConstrNum (undefined :: c a)

insertList' :: forall m a.(MonadBaseControl IO m, MonadIO m, PersistField a) => [a] -> DbPersist Postgresql m Int64
insertList' (l :: [a]) = do
  let mainName = "List" <> delim' <> delim' <> fromString (persistName (undefined :: a))
  k <- queryRawCached' ("INSERT INTO " <> escapeS mainName <> " DEFAULT VALUES RETURNING(id)") [] getKey
  let valuesName = mainName <> delim' <> "values"
  let fields = [("ord", dbType (0 :: Int)), ("value", dbType (undefined :: a))]
  let query = "INSERT INTO " <> escapeS valuesName <> "(id," <> renderFields escapeS fields <> ")VALUES(?," <> renderFields (const $ fromChar '?') fields <> ")"
  let go :: Int -> [a] -> DbPersist Postgresql m ()
      go n (x:xs) = do
       x' <- toPersistValues x
       executeRawCached' query $ (k:) . (toPrimitivePersistValue proxy n:) . x' $ []
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
  queryRawCached' query [toPrimitivePersistValue proxy k] $ mapAllRows (liftM fst . fromPersistValues)

--TODO: consider removal
{-# SPECIALIZE getKey :: RowPopper (DbPersist Postgresql IO) -> DbPersist Postgresql IO PersistValue #-}
getKey :: MonadIO m => RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m PersistValue
getKey pop = pop >>= \(Just [k]) -> return k

----------

executeRaw' :: MonadIO m => StringS -> [PersistValue] -> DbPersist Postgresql m ()
executeRaw' query vals = do
  --liftIO $ print $ fromStringS query ""
  Postgresql conn <- DbPersist ask
  let stmt = getStatement query
  liftIO $ do
    _ <- PG.execute conn stmt (map P vals)
    return ()

executeRawCached' :: MonadIO m => StringS -> [PersistValue] -> DbPersist Postgresql m ()
executeRawCached' = executeRaw'

queryRawCached' :: (MonadBaseControl IO m, MonadIO m) => StringS -> [PersistValue] -> (RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m a) -> DbPersist Postgresql m a
queryRawCached' = queryRaw'

renderCond' :: (PersistEntity v, Constructor c) => Cond v c -> Maybe (RenderS StringS)
renderCond' = renderCond proxy escapeS renderEquals renderNotEquals where
  renderEquals a b = a <> " IS NOT DISTINCT FROM " <> b
  renderNotEquals a b = a <> " IS DISTINCT FROM " <> b

escapeS :: StringS -> StringS
escapeS a = let q = fromChar '"' in q <> a <> q

delim' :: StringS
delim' = fromChar delim

toEntityPersistValues' :: (MonadBaseControl IO m, MonadIO m, PersistEntity v) => v -> DbPersist Postgresql m [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

constrId :: ConstructorDef -> Maybe StringS
constrId = fmap (escapeS . fromString) . constrAutoKeyName
