{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings, RecordWildCards, Rank2Types #-}

-- | This helper module contains generic versions of PersistBackend functions
module Database.Groundhog.Generic.PersistBackendHelpers
  ( 
    get
  , select
  , selectAll
  , getBy
  , project
  , count
  , replace
  , update
  , delete
  , insertByAll
  , deleteByKey
  , countAll
  , insertBy
  ) where

import Database.Groundhog.Core hiding (PersistBackend(..))
import Database.Groundhog.Core (PersistBackend, PhantomDb)
import qualified Database.Groundhog.Core as Core
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql

import Control.Monad (liftM, forM, (>=>))
import Data.Maybe (fromJust)
import Data.Monoid

{-# INLINABLE get #-}
get :: forall m s v . (PersistBackend m, StringLike s, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
    => (s -> s) -> (forall a . s -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> Key v BackendSpecific -> m (Maybe v)
get escape queryFunc (k :: Key v BackendSpecific) = do
  let e = entityDef (undefined :: v)
  let proxy = undefined :: Proxy (PhantomDb m)
  let name = persistName (undefined :: v)
  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let fields = renderFields escape (constrParams constr)
      let query = "SELECT " <> fields <> " FROM " <> escape (fromString name) <> " WHERE " <> fromJust (constrId escape constr) <> "=?"
      let types = getConstructorTypes constr
      x <- queryFunc query types [toPrimitivePersistValue proxy k] id
      case x of
        Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
        Nothing -> return Nothing
    else do
      let query = "SELECT discr FROM " <> escape (fromString name) <> " WHERE id=?"
      x <- queryFunc query [DbInt32] [toPrimitivePersistValue proxy k] id
      case x of
        Just [discr] -> do
          let constructorNum = fromPrimitivePersistValue proxy discr
          let constr = constructors e !! constructorNum
          let cName = fromString $ name ++ [delim] ++ constrName constr
          let fields = renderFields escape (constrParams constr)
          let cQuery = "SELECT " <> fields <> " FROM " <> escape cName <> " WHERE " <> fromJust (constrId escape constr) <> "=?"
          x2 <- queryFunc cQuery (getConstructorTypes constr) [toPrimitivePersistValue proxy k] id
          case x2 of
            Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ discr:xs
            Nothing -> fail "Missing entry in constructor table"
        Just x' -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing

select :: forall m s v c opts . (PersistBackend m, StringLike s, PersistEntity v, Constructor c, HasSelectOptions opts v c)
       => (s -> s) -> (forall a . s -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> s -> (Cond v c -> Maybe (RenderS s)) -> opts -> m [v]
select escape queryFunc noLimit renderCond' options = start where
  SelectOptions cond limit offset ords = getSelectOptions options
  start = if isSimple (constructors e)
    then doSelectQuery (mkQuery name) (0 :: Int)
    else let
      cName = name ++ [delim] ++ constrName constr
      in doSelectQuery (mkQuery cName) $ constrNum constr

  e = entityDef (undefined :: v)
  proxy = undefined :: Proxy (PhantomDb m)
  orders = renderOrders escape ords
  name = persistName (undefined :: v)
  (lim, limps) = case (limit, offset) of
        (Nothing, Nothing) -> ("", [])
        (Nothing, o) -> (" " <> noLimit <> " OFFSET ?", [toPrimitivePersistValue proxy o])
        (l, Nothing) -> (" LIMIT ?", [toPrimitivePersistValue proxy l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrimitivePersistValue proxy l, toPrimitivePersistValue proxy o])
  cond' = renderCond' cond
  fields = renderFields escape (constrParams constr)
  mkQuery tname = "SELECT " <> fields <> " FROM " <> escape (fromString tname) <> whereClause <> orders <> lim
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  doSelectQuery query cNum = queryFunc query types binds $ mapAllRows $ liftM fst . fromEntityPersistValues . (toPrimitivePersistValue proxy cNum:)
  binds = maybe id getValues cond' $ limps
  constr = constructors e !! phantomConstrNum (undefined :: c a)
  types = getConstructorTypes constr

selectAll :: forall m s v . (PersistBackend m, StringLike s, PersistEntity v)
          => (s -> s) -> (forall a . s -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> m [(AutoKey v, v)]
selectAll escape queryFunc = start where
  start = if isSimple (constructors e)
    then let
      constr = head $ constructors e
      fields = maybe id (\key cont -> key <> fromChar ',' <> cont) (constrId escape constr) $ renderFields escape (constrParams constr)
      query = "SELECT " <> fields <> " FROM " <> escape (fromString name)
      types = maybe id (const $ (DbInt64:)) (constrId escape constr) $ getConstructorTypes constr
      in queryFunc query types [] $ mapAllRows $ mkEntity proxy 0
    else liftM concat $ forM (zip [0..] (constructors e)) $ \(cNum, constr) -> do
        let fields = fromJust (constrId escape constr) <> fromChar ',' <> renderFields escape (constrParams constr)
        let cName = fromString $ name ++ [delim] ++ constrName constr
        let query = "SELECT " <> fields <> " FROM " <> escape cName
        let types = DbInt64:getConstructorTypes constr
        queryFunc query types [] $ mapAllRows $ mkEntity proxy cNum
  e = entityDef (undefined :: v)
  proxy = undefined :: Proxy (PhantomDb m)
  name = persistName (undefined :: v)

getBy :: forall m s v u . (PersistBackend m, StringLike s, PersistEntity v, IsUniqueKey (Key v (Unique u)))
      => (s -> s) -> (forall a . s -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> Key v (Unique u) -> m (Maybe v)
getBy escape queryFunc (k :: Key v (Unique u)) = do
  let e = entityDef (undefined :: v)
  let name = persistName (undefined :: v)
  uniques <- toPersistValues k
  let u = (undefined :: Key v (Unique u) -> u (UniqueMarker v)) k
  let uFields = foldr (renderChain escape) [] $ projectionFieldChains u []
  let cond = intercalateS " AND " $ map (<> "=?") uFields
  let constr = head $ constructors e
  let fields = renderFields escape (constrParams constr)
  let query = "SELECT " <> fields <> " FROM " <> escape (fromString name) <> " WHERE " <> cond
  x <- queryFunc query (getConstructorTypes constr) (uniques []) id
  case x of
    Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
    Nothing -> return Nothing

project :: forall m s v c p opts a'. (PersistBackend m, StringLike s, PersistEntity v, Constructor c, Projection p (RestrictionHolder v c) a', HasSelectOptions opts v c)
        => (s -> s) -> (forall a . s -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> s -> (Cond v c -> Maybe (RenderS s)) -> p -> opts -> m [a']
project escape queryFunc noLimit renderCond' p options = start where
  SelectOptions cond limit offset ords = getSelectOptions options
  start = doSelectQuery $ if isSimple (constructors e)
    then mkQuery name
    else let
      cName = name ++ [delim] ++ constrName constr
      in mkQuery cName

  e = entityDef (undefined :: v)
  proxy = undefined :: Proxy (PhantomDb m)
  orders = renderOrders escape ords
  name = persistName (undefined :: v)
  (lim, limps) = case (limit, offset) of
        (Nothing, Nothing) -> ("", [])
        (Nothing, o) -> (" " <> noLimit <> " OFFSET ?", [toPrimitivePersistValue proxy o])
        (l, Nothing) -> (" LIMIT ?", [toPrimitivePersistValue proxy l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrimitivePersistValue proxy l, toPrimitivePersistValue proxy o])
  cond' = renderCond' cond
  chains = projectionFieldChains p []
  fields = intercalateS (fromChar ',') $ foldr (renderChain escape) [] chains
  mkQuery tname = "SELECT " <> fields <> " FROM " <> escape (fromString tname) <> whereClause <> orders <> lim
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  doSelectQuery query = queryFunc query types binds $ mapAllRows $ liftM fst . projectionResult p
  binds = maybe id getValues cond' $ limps
  constr = constructors e !! phantomConstrNum (undefined :: c a)
  types = foldr getDbTypes [] $ map (snd . fst) chains

count :: forall m s v c . (PersistBackend m, StringLike s, PersistEntity v, Constructor c)
      => (s -> s) -> (forall a . s -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> (Cond v c -> Maybe (RenderS s)) -> Cond v c -> m Int
count escape queryFunc renderCond' (cond :: Cond v c) = do
  let e = entityDef (undefined :: v)
  let proxy = undefined :: Proxy (PhantomDb m)
  let cond' = renderCond' cond
  let name = persistName (undefined :: v)
  let tname = fromString $ if isSimple (constructors e)
       then name
       else name ++ [delim] ++ phantomConstrName (undefined :: c a)
  let query = "SELECT COUNT(*) FROM " <> escape tname <> whereClause where
      whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  x <- queryFunc query [DbInt32] (maybe [] (flip getValues []) cond') id
  case x of
    Just [num] -> return $ fromPrimitivePersistValue proxy num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

replace :: forall m s v . (PersistBackend m, StringLike s, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
        => (s -> s) -> (forall a . s -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> (s -> [PersistValue] -> m ()) -> (Bool -> String -> ConstructorDef -> s)
        -> Key v BackendSpecific -> v -> m ()
replace escape queryFunc execFunc insertIntoConstructorTable k v = do
  vals <- toEntityPersistValues' v
  let e = entityDef v
  let proxy = undefined :: Proxy (PhantomDb m)
  let name = persistName v
  let constructorNum = fromPrimitivePersistValue proxy (head vals)
  let constr = constructors e !! constructorNum

  let upds = renderFields (\f -> escape f <> "=?") $ constrParams constr
  let mkQuery tname = "UPDATE " <> escape (fromString tname) <> " SET " <> upds <> " WHERE " <> fromString (fromJust $ constrAutoKeyName constr) <> "=?"

  if isSimple (constructors e)
    then execFunc (mkQuery name) (tail vals ++ [toPrimitivePersistValue proxy k])
    else do
      let query = "SELECT discr FROM " <> escape (fromString name) <> " WHERE id=?"
      x <- queryFunc query [DbInt32] [toPrimitivePersistValue proxy k] (id >=> return . fmap (fromPrimitivePersistValue proxy . head))
      case x of
        Just discr -> do
          let cName = name ++ [delim] ++ constrName constr

          if discr == constructorNum
            then execFunc (mkQuery cName) (tail vals ++ [toPrimitivePersistValue proxy k])
            else do
              let insQuery = insertIntoConstructorTable True cName constr
              execFunc insQuery (toPrimitivePersistValue proxy k:tail vals)

              let oldCName = fromString $ name ++ [delim] ++ constrName (constructors e !! discr)
              let delQuery = "DELETE FROM " <> escape oldCName <> " WHERE " <> fromJust (constrId escape constr) <> "=?"
              execFunc delQuery [toPrimitivePersistValue proxy k]

              let updateDiscrQuery = "UPDATE " <> escape (fromString name) <> " SET discr=? WHERE id=?"
              execFunc updateDiscrQuery [head vals, toPrimitivePersistValue proxy k]
        Nothing -> return ()

update :: forall m s v c . (PersistBackend m, StringLike s, PersistEntity v, Constructor c)
       => (s -> s) -> (s -> [PersistValue] -> m ()) -> (Cond v c -> Maybe (RenderS s)) -> [Update v c] -> Cond v c -> m ()
update escape execFunc renderCond' upds (cond :: Cond v c) = do
  let e = entityDef (undefined :: v)
  let proxy = undefined :: Proxy (PhantomDb m)
  let name = persistName (undefined :: v)
  case renderUpdates proxy escape upds of
    Just upds' -> do
      let cond' = renderCond' cond
      let mkQuery tname = "UPDATE " <> escape tname <> " SET " <> whereClause where
          whereClause = maybe (getQuery upds') (\c -> getQuery upds' <> " WHERE " <> getQuery c) cond'
      let tname = fromString $ if isSimple (constructors e) then name else name ++ [delim] ++ phantomConstrName (undefined :: c a)
      execFunc (mkQuery tname) (getValues upds' <> maybe mempty getValues cond' $ [])
    Nothing -> return ()

delete :: forall m s v c . (PersistBackend m, StringLike s, PersistEntity v, Constructor c)
       => (s -> s) -> (s -> [PersistValue] -> m ()) -> (Cond v c -> Maybe (RenderS s)) -> Cond v c -> m ()
delete escape execFunc renderCond' (cond :: Cond v c) = execFunc query (maybe [] (($ []) . getValues) cond') where
  e = entityDef (undefined :: v)
  constr = head $ constructors e
  cond' = renderCond' cond
  name = persistName (undefined :: v)
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  query = if isSimple (constructors e)
    then "DELETE FROM " <> escape (fromString name) <> whereClause
    -- the entries in the constructor table are deleted because of the reference on delete cascade
    else "DELETE FROM " <> escape (fromString name) <> " WHERE id IN(SELECT " <> fromJust (constrId escape constr) <> " FROM " <> escape cName <> whereClause <> ")" where
      cName = fromString $ name ++ [delim] ++ phantomConstrName (undefined :: c a)

insertByAll :: forall m s v . (PersistBackend m, StringLike s, PersistEntity v)
            => (s -> s) -> (forall a . s -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a)
            -> v -> m (Either (AutoKey v) (AutoKey v))
insertByAll escape queryFunc v = do
  let e = entityDef v
  let proxy = undefined :: Proxy (PhantomDb m)
  let name = persistName v

  let (constructorNum, uniques) = getUniques proxy v
  let uniqueDefs = constrUniques $ constructors e !! constructorNum
  let cond = intercalateS " OR " $ map (intercalateS " AND " . map (\(fname, _) -> escape (fromString fname) <> "=?")) $ map (\(UniqueDef _ _ fields) -> fields) uniqueDefs

  let ifAbsent tname constr = do
      let query = "SELECT " <> maybe "1" id (constrId escape constr) <> " FROM " <> escape (fromString tname) <> " WHERE " <> cond
      x <- queryFunc query [DbInt64] (foldr ((.) . snd) id uniques []) id
      case x of
        Nothing  -> liftM Right $ Core.insert v
        Just [k] -> return $ Left $ fst $ fromPurePersistValues proxy [k]
        Just xs  -> fail $ "unexpected query result: " ++ show xs
  if null uniques
    then liftM Right $ Core.insert v
    else if isSimple (constructors e)
      then do
        let constr = head $ constructors e
        ifAbsent name constr
      else do
        let constr = constructors e !! constructorNum
        let cName = name ++ [delim] ++ constrName constr
        ifAbsent cName constr

deleteByKey :: forall m s v . (PersistBackend m, StringLike s, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
            => (s -> s) -> (s -> [PersistValue] -> m ()) -> Key v BackendSpecific -> m ()
deleteByKey escape execFunc k = execFunc query [toPrimitivePersistValue proxy k] where
  e = entityDef ((undefined :: Key v u -> v) k)
  proxy = undefined :: Proxy (PhantomDb m)
  constr = head $ constructors e
  name = fromString (persistName $ (undefined :: Key v u -> v) k)
  query = "DELETE FROM " <> escape name <> " WHERE " <> fromJust (constrId escape constr) <> "=?"

countAll :: forall m s v . (PersistBackend m, StringLike s, PersistEntity v)
         => (s -> s) -> (forall a . s -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> v -> m Int
countAll escape queryFunc (_ :: v) = do
  let proxy = undefined :: Proxy (PhantomDb m)
  let name = persistName (undefined :: v)
  let query = "SELECT COUNT(*) FROM " <> escape (fromString name)
  x <- queryFunc query [DbInt64] [] id
  case x of
    Just [num] -> return $ fromPrimitivePersistValue proxy num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

insertBy :: forall m s v u . (PersistBackend m, StringLike s, PersistEntity v, IsUniqueKey (Key v (Unique u)))
         => (s -> s) -> (forall a . s -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a)
         -> u (UniqueMarker v) -> v -> m (Either (AutoKey v) (AutoKey v))
insertBy escape queryFunc u v = do
  let e = entityDef v
  let proxy = undefined :: Proxy (PhantomDb m)
  let name = persistName v
  uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
  let fields = foldr (renderChain escape) [] $ projectionFieldChains u []
  let cond = intercalateS " AND " $ map (<> "=?") fields
  
  let ifAbsent tname constr = do
      let query = "SELECT " <> maybe "1" id (constrId escape constr) <> " FROM " <> escape (fromString tname) <> " WHERE " <> cond
      x <- queryFunc query [DbInt64] (uniques []) id
      case x of
        Nothing  -> liftM Right $ Core.insert v
        Just [k] -> return $ Left $ fst $ fromPurePersistValues proxy [k]
        Just xs  -> fail $ "unexpected query result: " ++ show xs
  let constr = head $ constructors e
  ifAbsent name constr

getConstructorTypes :: ConstructorDef -> [DbType]
getConstructorTypes = foldr getDbTypes [] . map snd . constrParams where

getDbTypes :: DbType -> [DbType] -> [DbType]
getDbTypes typ acc = case typ of
  DbEmbedded (EmbeddedDef _ ts) -> foldr (getDbTypes . snd) acc ts
  DbEntity (Just (EmbeddedDef _ ts, _)) _ -> foldr (getDbTypes . snd) acc ts
  t               -> t:acc

constrId :: StringLike s => (s -> s) -> ConstructorDef -> Maybe s
constrId escape = fmap (escape . fromString) . constrAutoKeyName

-- | receives constructor number and row of values from the constructor table
mkEntity :: (PersistEntity v, PersistBackend m) => Proxy (PhantomDb m) -> Int -> [PersistValue] -> m (AutoKey v, v)
mkEntity proxy i xs = fromEntityPersistValues (toPrimitivePersistValue proxy i:xs') >>= \(v, _) -> return (k, v) where
  (k, xs') = fromPurePersistValues proxy xs

toEntityPersistValues' :: (PersistBackend m, PersistEntity v) => v -> m [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues
