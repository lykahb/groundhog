{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings, RecordWildCards, Rank2Types, TypeFamilies, ConstraintKinds #-}

-- | This helper module contains generic versions of PersistBackend functions
module Database.Groundhog.Generic.PersistBackendHelpers
  ( 
    get
  , select
  , selectAll
  , selectStream
  , selectAllStream
  , getBy
  , project
  , projectStream
  , count
  , replace
  , replaceBy
  , update
  , delete
  , deleteBy
  , deleteAll
  , insertByAll
  , countAll
  , insertBy
  ) where

import Database.Groundhog.Core hiding (PersistBackend(..))
import Database.Groundhog.Core (PersistBackend, PhantomDb)
import qualified Database.Groundhog.Core as Core
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql

import Control.Monad (liftM, (>=>))
import Data.Either (rights)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Monoid

{-# INLINABLE get #-}
get :: forall m v . (PersistBackend m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
    => RenderConfig -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> Key v BackendSpecific -> m (Maybe v)
get RenderConfig{..} queryFunc (k :: Key v BackendSpecific) = do
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy (PhantomDb m)
  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let fields = renderFields esc (constrParams constr)
      let query = "SELECT " <> fields <> " FROM " <> tableName esc e constr <> " WHERE " <> fromJust (constrId esc constr) <> "=?"
      x <- queryFunc query [toPrimitivePersistValue proxy k] id
      case x of
        Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
        Nothing -> return Nothing
    else do
      let query = "SELECT discr FROM " <> mainTableName esc e <> " WHERE id=?"
      x <- queryFunc query [toPrimitivePersistValue proxy k] id
      case x of
        Just [discr] -> do
          let constructorNum = fromPrimitivePersistValue proxy discr
              constr = constructors e !! constructorNum
              fields = renderFields esc (constrParams constr)
              cQuery = "SELECT " <> fields <> " FROM " <> tableName esc e constr <> " WHERE " <> fromJust (constrId esc constr) <> "=?"
          x2 <- queryFunc cQuery [toPrimitivePersistValue proxy k] id
          case x2 of
            Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ discr:xs
            Nothing -> fail "Missing entry in constructor table"
        Just x' -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing

popperToList :: Monad m => m (Maybe a) -> m [a]
popperToList pop = go where
  go = pop >>= maybe (return []) (\a -> liftM (a:) go)

mapPopper :: Monad m => (a -> m b) -> m (Maybe a) -> m (Maybe b)
mapPopper f pop = pop >>= \a -> case a of
  Nothing -> return Nothing
  Just a' -> liftM Just $ f a'

select :: forall m db r v c opts . (SqlDb db, db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c, HasSelectOptions opts db r)
       => RenderConfig -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> (opts -> RenderS db r) -> Utf8 -> opts -> m [v]
select conf queryFunc preColumns noLimit options = selectStream conf queryFunc preColumns noLimit options popperToList

selectStream :: forall m db r v c opts result . (SqlDb db, db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c, HasSelectOptions opts db r)
       => RenderConfig -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> (opts -> RenderS db r) -> Utf8 -> opts -> (m (Maybe v) -> m result) -> m result
selectStream conf@RenderConfig{..} queryFunc preColumns noLimit options f = doSelectQuery where
  SelectOptions cond limit offset ords dist _ = getSelectOptions options

  e = entityDef proxy (undefined :: v)
  proxy = undefined :: proxy (PhantomDb m)
  orders = renderOrders conf ords
  lim = case (limit, offset) of
          (Nothing, Nothing) -> mempty
          (Nothing, o) -> RenderS (" " <> noLimit <> " OFFSET ?") (toPurePersistValues proxy o)
          (l, Nothing) -> RenderS " LIMIT ?" (toPurePersistValues proxy l)
          (l, o) -> RenderS " LIMIT ? OFFSET ?" (toPurePersistValues proxy (l, o))
  cond' = renderCond conf cond
  fields = RenderS (renderFields esc (constrParams constr)) id
  distinctClause = if dist then "DISTINCT " else mempty
  RenderS query binds = "SELECT " <> distinctClause <> preColumns options <> fields <> " FROM " <> RenderS (tableName esc e constr) id <> whereClause <> orders <> lim
  whereClause = maybe "" (" WHERE " <>) cond'
  doSelectQuery = queryFunc query (binds []) $ f . mapPopper (\xs -> liftM fst $ fromEntityPersistValues $ (toPrimitivePersistValue proxy cNum):xs)
  cNum = entityConstrNum (undefined :: proxy v) (undefined :: c a)
  constr = constructors e !! cNum

selectAll :: forall m v . (PersistBackend m, PersistEntity v)
          => RenderConfig -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> m [(AutoKey v, v)]
selectAll conf queryFunc = result where
  result = fmap (foldr ($) []) $ selectAllStream conf queryFunc popperToDiffList
  popperToDiffList pop = go where
    go = pop >>= maybe (return id) (\a -> liftM ((a:) .) go)

-- | It may call the passed function multiple times
selectAllStream :: forall m v result . (PersistBackend m, PersistEntity v)
                => RenderConfig -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> (m (Maybe (AutoKey v, v)) -> m result) -> m [result]
selectAllStream RenderConfig{..} queryFunc f = start where
  start = sequence $ zipWith selectConstr [0..] $ constructors e
  selectConstr cNum constr = queryFunc query [] $ f . mkEntity cNum where
    fields = maybe id (\key cont -> key <> fromChar ',' <> cont) (constrId esc constr) $ renderFields esc (constrParams constr)
    query = "SELECT " <> fields <> " FROM " <> tableName esc e constr
  e = entityDef proxy (undefined :: v)
  proxy = undefined :: proxy (PhantomDb m)
  mkEntity cNum = mapPopper $ \xs -> do
    let (k, xs') = fromPurePersistValues proxy xs
    (v, _) <- fromEntityPersistValues (toPrimitivePersistValue proxy (cNum :: Int):xs')
    return (k, v)

getBy :: forall m v u . (PersistBackend m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
      => RenderConfig
      -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -- ^ function to run query
      -> Key v (Unique u)
      -> m (Maybe v)
getBy conf@RenderConfig{..} queryFunc (k :: Key v (Unique u)) = do
  uniques <- toPersistValues k
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy (PhantomDb m)
      u = (undefined :: Key v (Unique u) -> u (UniqueMarker v)) k
      uFields = renderChain conf (fieldChain proxy u) []
      RenderS cond vals = intercalateS " AND " $ mkUniqueCond uFields uniques
      constr = head $ constructors e
      fields = renderFields esc (constrParams constr)
      query = "SELECT " <> fields <> " FROM " <> tableName esc e constr <> " WHERE " <> cond
  x <- queryFunc query (vals []) id
  case x of
    Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
    Nothing -> return Nothing

project :: forall m db r v c p opts a'. (SqlDb db, db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c, Projection p a', ProjectionDb p db, ProjectionRestriction p r, HasSelectOptions opts db r)
        => RenderConfig -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> (opts -> RenderS db r) -> Utf8 -> p -> opts -> m [a']
project conf queryFunc preColumns noLimit p options = projectStream conf queryFunc preColumns noLimit p options popperToList

projectStream :: forall m db r v c p opts a' result . (SqlDb db, db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c, Projection p a', ProjectionDb p db, ProjectionRestriction p r, HasSelectOptions opts db r)
        => RenderConfig -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> (opts -> RenderS db r) -> Utf8 -> p -> opts -> (m (Maybe a') -> m result) -> m result
projectStream conf@RenderConfig{..} queryFunc preColumns noLimit p options f = doSelectQuery where
  SelectOptions cond limit offset ords dist _ = getSelectOptions options
  e = entityDef proxy (undefined :: v)
  proxy = undefined :: proxy (PhantomDb m)
  orders = renderOrders conf ords
  lim = case (limit, offset) of
          (Nothing, Nothing) -> mempty
          (Nothing, o) -> RenderS (" " <> noLimit <> " OFFSET ?") (toPurePersistValues proxy o)
          (l, Nothing) -> RenderS " LIMIT ?" (toPurePersistValues proxy l)
          (l, o) -> RenderS " LIMIT ? OFFSET ?" (toPurePersistValues proxy (l, o))
  cond' = renderCond conf cond
  chains = projectionExprs p []
  fields = commasJoin $ concatMap (renderExprExtended conf 0) chains
  distinctClause = if dist then "DISTINCT " else mempty
  RenderS query binds = "SELECT " <> distinctClause <> preColumns options <> fields <> " FROM " <> RenderS (tableName esc e constr) id <> whereClause <> orders <> lim
  whereClause = maybe "" (" WHERE " <>) cond'
  doSelectQuery = queryFunc query (binds []) $ f . mapPopper (liftM fst . projectionResult p)
  constr = constructors e !! entityConstrNum (undefined :: proxy v) (undefined :: c a)

count :: forall m db r v c . (SqlDb db, db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c)
      => RenderConfig -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> Cond db r -> m Int
count conf@RenderConfig{..} queryFunc cond = do
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy (PhantomDb m)
      cond' = renderCond conf cond
      constr = constructors e !! entityConstrNum (undefined :: proxy v) (undefined :: c a)
      query = "SELECT COUNT(*) FROM " <> tableName esc e constr <> whereClause where
      whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  x <- queryFunc query (maybe [] (flip getValues []) cond') id
  case x of
    Just [num] -> return $ fromPrimitivePersistValue proxy num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

replace :: forall m db r v . (PersistBackend m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
        => RenderConfig -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> (Utf8 -> [PersistValue] -> m ()) -> (Bool -> Utf8 -> ConstructorDef -> [PersistValue] -> RenderS db r)
        -> Key v BackendSpecific -> v -> m ()
replace RenderConfig{..} queryFunc execFunc insertIntoConstructorTable k v = do
  vals <- toEntityPersistValues' v
  let e = entityDef proxy v
      proxy = undefined :: proxy (PhantomDb m)
      constructorNum = fromPrimitivePersistValue proxy (head vals)
      constr = constructors e !! constructorNum
      k' = toPrimitivePersistValue proxy k
      RenderS upds updsVals = commasJoin $ zipWith f fields $ tail vals where
        fields = foldr (flatten esc) [] $ constrParams constr
        f f1 f2 = RenderS f1 id <> fromChar '=' <> renderPersistValue f2
      updateQuery = "UPDATE " <> tableName esc e constr <> " SET " <> upds <> " WHERE " <> fromString (fromJust $ constrAutoKeyName constr) <> "=?"

  if isSimple (constructors e)
    then execFunc updateQuery (updsVals [k'])
    else do
      let query = "SELECT discr FROM " <> mainTableName esc e <> " WHERE id=?"
      x <- queryFunc query [k'] (id >=> return . fmap (fromPrimitivePersistValue proxy . head))
      case x of
        Just discr -> do
          let cName = tableName esc e constr

          if discr == constructorNum
            then execFunc updateQuery (updsVals [k'])
            else do
              let RenderS insQuery vals' = insertIntoConstructorTable True cName constr (k':tail vals)
              execFunc insQuery (vals' [])

              let oldConstr = constructors e !! discr
              let delQuery = "DELETE FROM " <> tableName esc e oldConstr <> " WHERE " <> fromJust (constrId esc oldConstr) <> "=?"
              execFunc delQuery [k']

              let updateDiscrQuery = "UPDATE " <> mainTableName esc e <> " SET discr=? WHERE id=?"
              execFunc updateDiscrQuery [head vals, k']
        Nothing -> return ()

replaceBy :: forall m v u . (PersistBackend m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
          => RenderConfig
          -> (Utf8 -> [PersistValue] -> m ()) -- ^ function to execute query
          -> u (UniqueMarker v)
          -> v
          -> m ()
replaceBy conf@RenderConfig{..} execFunc u v = do
  uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
  vals <- toEntityPersistValues' v
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy (PhantomDb m)
      uFields = renderChain conf (fieldChain proxy u) []
      RenderS cond condVals = intercalateS " AND " $ mkUniqueCond uFields uniques
      constr = head $ constructors e
      RenderS upds updsVals = commasJoin $ zipWith f fields $ tail vals where
        fields = foldr (flatten esc) [] $ constrParams constr
        f f1 f2 = RenderS f1 id <> fromChar '=' <> renderPersistValue f2
      updateQuery = "UPDATE " <> tableName esc e constr <> " SET " <> upds <> " WHERE " <> cond
  execFunc updateQuery (updsVals . condVals $ [])

update :: forall m db r v c . (SqlDb db, db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c)
       => RenderConfig -> (Utf8 -> [PersistValue] -> m ()) -> [Update db r] -> Cond db r -> m ()
update conf@RenderConfig{..} execFunc upds cond = do
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy (PhantomDb m)
  case renderUpdates conf upds of
    Just upds' -> do
      let cond' = renderCond conf cond
          constr = constructors e !! entityConstrNum (undefined :: proxy v) (undefined :: c a)
          query = "UPDATE " <> tableName esc e constr <> " SET " <> whereClause where
          whereClause = maybe (getQuery upds') (\c -> getQuery upds' <> " WHERE " <> getQuery c) cond'
      execFunc query (getValues upds' <> maybe mempty getValues cond' $ [])
    Nothing -> return ()

delete :: forall m db r v c . (SqlDb db, db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c)
       => RenderConfig -> (Utf8 -> [PersistValue] -> m ()) -> Cond db r -> m ()
delete conf@RenderConfig{..} execFunc cond = execFunc query (maybe [] (($ []) . getValues) cond') where
  e = entityDef proxy (undefined :: v)
  proxy = undefined :: proxy (PhantomDb m)
  constr = constructors e !! entityConstrNum (undefined :: proxy v) (undefined :: c a)
  cond' = renderCond conf cond
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  query = if isSimple (constructors e)
    then "DELETE FROM " <> tableName esc e constr <> whereClause
    -- the entries in the constructor table are deleted because of the reference on delete cascade
    else "DELETE FROM " <> mainTableName esc e <> " WHERE id IN(SELECT " <> fromJust (constrId esc constr) <> " FROM " <> tableName esc e constr <> whereClause <> ")"

insertByAll :: forall m v . (PersistBackend m, PersistEntity v)
            => RenderConfig
            -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -- ^ function to run query
            -> Bool -- ^ allow multiple duplication of uniques with nulls
            -> v -> m (Either (AutoKey v) (AutoKey v))
insertByAll RenderConfig{..} queryFunc manyNulls v = do
  let e = entityDef proxy v
      proxy = undefined :: proxy (PhantomDb m)
      (constructorNum, uniques) = getUniques proxy v
      constr = constructors e !! constructorNum
      uniqueDefs = constrUniques constr

      query = "SELECT " <> maybe "1" id (constrId esc constr) <> " FROM " <> tableName esc e constr <> " WHERE " <> cond
      conds = catMaybes $ zipWith (\uFields (_, uVals) -> checkNulls uVals $ intercalateS " AND " $ mkUniqueCond uFields uVals) (mapMaybe f uniqueDefs) uniques where
        f u@(UniqueDef _ _ uFields) = if null $ rights uFields
          then Just $ foldr (flatten esc) [] $ getUniqueFields u
          else Nothing
      -- skip condition if any value is NULL. It allows to insert many values with duplicate unique key
      checkNulls uVals x = if manyNulls && any (== PersistNull) (uVals []) then Nothing else Just x
      RenderS cond vals = intercalateS " OR " conds
  if null conds
    then liftM Right $ Core.insert v
    else do
      x <- queryFunc query (vals []) id
      case x of
        Nothing -> liftM Right $ Core.insert v
        Just xs -> return $ Left $ fst $ fromPurePersistValues proxy xs

deleteBy :: forall m v . (PersistBackend m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
            => RenderConfig -> (Utf8 -> [PersistValue] -> m ()) -> Key v BackendSpecific -> m ()
deleteBy RenderConfig{..} execFunc k = execFunc query [toPrimitivePersistValue proxy k] where
  e = entityDef proxy ((undefined :: Key v u -> v) k)
  proxy = undefined :: proxy (PhantomDb m)
  constr = head $ constructors e
  idName = if isSimple (constructors e)
    then fromJust $ constrId esc constr
    else "id"
  -- the entries in the constructor table are deleted because of the reference on delete cascade
  query = "DELETE FROM " <> mainTableName esc e <> " WHERE " <> idName <> "=?"

deleteAll :: forall m v . (PersistBackend m, PersistEntity v)
          => RenderConfig -> (Utf8 -> [PersistValue] -> m ()) -> v -> m ()
deleteAll RenderConfig{..} execFunc (_ :: v) = execFunc query [] where
  e = entityDef proxy (undefined :: v)
  proxy = undefined :: proxy (PhantomDb m)
  query = "DELETE FROM " <> mainTableName esc e

countAll :: forall m v . (PersistBackend m, PersistEntity v)
         => RenderConfig -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> v -> m Int
countAll RenderConfig{..} queryFunc (_ :: v) = do
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy (PhantomDb m)
      query = "SELECT COUNT(*) FROM " <> mainTableName esc e
  x <- queryFunc query [] id
  case x of
    Just [num] -> return $ fromPrimitivePersistValue proxy num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

insertBy :: forall m v u . (PersistBackend m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
         => RenderConfig
         -> (forall a . Utf8 -> [PersistValue] -> (RowPopper m -> m a) -> m a)
         -> Bool
         -> u (UniqueMarker v) -> v -> m (Either (AutoKey v) (AutoKey v))
insertBy conf@RenderConfig{..} queryFunc manyNulls u v = do
  uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
  let e = entityDef proxy v
      proxy = undefined :: proxy (PhantomDb m)
      uFields = renderChain conf (fieldChain proxy u) []
      RenderS cond vals = intercalateS " AND " $ mkUniqueCond uFields uniques
      -- skip condition if any value is NULL. It allows to insert many values with duplicate unique key
      checkNulls uVals = manyNulls && any (== PersistNull) (uVals [])
      -- this is safe because unique keys exist only for entities with one constructor
      constr = head $ constructors e
      query = "SELECT " <> maybe "1" id (constrId esc constr) <> " FROM " <> tableName esc e constr <> " WHERE " <> cond
  if checkNulls uniques
    then liftM Right $ Core.insert v
    else do
      x <- queryFunc query (vals []) id
      case x of
        Nothing  -> liftM Right $ Core.insert v
        Just [k] -> return $ Left $ fst $ fromPurePersistValues proxy [k]
        Just xs  -> fail $ "unexpected query result: " ++ show xs

constrId :: (Utf8 -> Utf8) -> ConstructorDef -> Maybe Utf8
constrId escape = fmap (escape . fromString) . constrAutoKeyName

toEntityPersistValues' :: (PersistBackend m, PersistEntity v) => v -> m [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

mkUniqueCond :: [Utf8] -> ([PersistValue] -> [PersistValue]) -> [RenderS db r]
mkUniqueCond u vals = zipWith f u (vals []) where
  f a PersistNull = RenderS (a <> " IS NULL") id
  f a x = RenderS (a <> "=?") (x:)
