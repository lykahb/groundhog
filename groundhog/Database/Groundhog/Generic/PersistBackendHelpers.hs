{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings, RecordWildCards, Rank2Types, TypeFamilies #-}

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

import Control.Monad (liftM, forM, (>=>))
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid

{-# INLINABLE get #-}
get :: forall m v . (PersistBackend m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
    => (Utf8 -> Utf8) -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> Key v BackendSpecific -> m (Maybe v)
get escape queryFunc (k :: Key v BackendSpecific) = do
  let e = entityDef (undefined :: v)
  let proxy = undefined :: Proxy (PhantomDb m)
  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let fields = renderFields escape (constrParams constr)
      let query = "SELECT " <> fields <> " FROM " <> tableName escape e constr <> " WHERE " <> fromJust (constrId escape constr) <> "=?"
      let types = getConstructorTypes constr
      x <- queryFunc query types [toPrimitivePersistValue proxy k] id
      case x of
        Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
        Nothing -> return Nothing
    else do
      let query = "SELECT discr FROM " <> mainTableName escape e <> " WHERE id=?"
      x <- queryFunc query [dbInt64] [toPrimitivePersistValue proxy k] id
      case x of
        Just [discr] -> do
          let constructorNum = fromPrimitivePersistValue proxy discr
              constr = constructors e !! constructorNum
              fields = renderFields escape (constrParams constr)
              cQuery = "SELECT " <> fields <> " FROM " <> tableName escape e constr <> " WHERE " <> fromJust (constrId escape constr) <> "=?"
          x2 <- queryFunc cQuery (getConstructorTypes constr) [toPrimitivePersistValue proxy k] id
          case x2 of
            Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ discr:xs
            Nothing -> fail "Missing entry in constructor table"
        Just x' -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing

select :: forall m db r v c opts . (db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c, HasSelectOptions opts db r)
       => (Utf8 -> Utf8) -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> Utf8 -> (Cond db r -> Maybe (RenderS db r)) -> opts -> m [v]
select escape queryFunc noLimit renderCond' options = doSelectQuery where
  SelectOptions cond limit offset ords = getSelectOptions options

  e = entityDef (undefined :: v)
  proxy = undefined :: Proxy (PhantomDb m)
  orders = renderOrders escape ords
  (lim, limps) = case (limit, offset) of
        (Nothing, Nothing) -> ("", [])
        (Nothing, o) -> (" " <> noLimit <> " OFFSET ?", [toPrimitivePersistValue proxy o])
        (l, Nothing) -> (" LIMIT ?", [toPrimitivePersistValue proxy l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrimitivePersistValue proxy l, toPrimitivePersistValue proxy o])
  cond' = renderCond' cond
  fields = renderFields escape (constrParams constr)
  query = "SELECT " <> fields <> " FROM " <> tableName escape e constr <> whereClause <> orders <> lim
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  doSelectQuery = queryFunc query types binds $ mapAllRows $ liftM fst . fromEntityPersistValues . (toPrimitivePersistValue proxy cNum:)
  binds = maybe id getValues cond' $ limps
  cNum = entityConstrNum (undefined :: Proxy v) (undefined :: c a)
  constr = constructors e !! cNum
  types = getConstructorTypes constr

selectAll :: forall m v . (PersistBackend m, PersistEntity v)
          => (Utf8 -> Utf8) -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> m [(AutoKey v, v)]
selectAll escape queryFunc = start where
  start = if isSimple (constructors e)
    then let
      constr = head $ constructors e
      fields = maybe id (\key cont -> key <> fromChar ',' <> cont) (constrId escape constr) $ renderFields escape (constrParams constr)
      query = "SELECT " <> fields <> " FROM " <> tableName escape e constr
      types = maybe id (const $ (dbInt64:)) (constrId escape constr) $ getConstructorTypes constr
      in queryFunc query types [] $ mapAllRows $ mkEntity proxy 0
    else liftM concat $ forM (zip [0..] (constructors e)) $ \(cNum, constr) -> do
        let fields = fromJust (constrId escape constr) <> fromChar ',' <> renderFields escape (constrParams constr)
        let query = "SELECT " <> fields <> " FROM " <> tableName escape e constr
        let types = dbInt64:getConstructorTypes constr
        queryFunc query types [] $ mapAllRows $ mkEntity proxy cNum
  e = entityDef (undefined :: v)
  proxy = undefined :: Proxy (PhantomDb m)

getBy :: forall m v u . (PersistBackend m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
      => (Utf8 -> Utf8) -- ^ escape
      -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -- ^ function to run query
      -> Key v (Unique u)
      -> m (Maybe v)
getBy escape queryFunc (k :: Key v (Unique u)) = do
  uniques <- toPersistValues k
  let e = entityDef (undefined :: v)
      u = (undefined :: Key v (Unique u) -> u (UniqueMarker v)) k
      uFields = renderChain escape (fieldChain u) []
      RenderS cond vals = intercalateS " AND " $ mkUniqueCond uFields uniques
      constr = head $ constructors e
      fields = renderFields escape (constrParams constr)
      query = "SELECT " <> fields <> " FROM " <> tableName escape e constr <> " WHERE " <> cond
  x <- queryFunc query (getConstructorTypes constr) (vals []) id
  case x of
    Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
    Nothing -> return Nothing

project :: forall m db r v c p opts a'. (SqlDb db, QueryRaw db ~ Snippet db, db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c, Projection p db r a', HasSelectOptions opts db r)
        => (Utf8 -> Utf8) -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> Utf8 -> (Cond db r -> Maybe (RenderS db r)) -> p -> opts -> m [a']
project escape queryFunc noLimit renderCond' p options = doSelectQuery where
  SelectOptions cond limit offset ords = getSelectOptions options
  e = entityDef (undefined :: v)
  proxy = undefined :: Proxy (PhantomDb m)
  orders = renderOrders escape ords
  (lim, limps) = case (limit, offset) of
        (Nothing, Nothing) -> ("", [])
        (Nothing, o) -> (" " <> noLimit <> " OFFSET ?", [toPrimitivePersistValue proxy o])
        (l, Nothing) -> (" LIMIT ?", [toPrimitivePersistValue proxy l])
        (l, o) -> (" LIMIT ? OFFSET ?", [toPrimitivePersistValue proxy l, toPrimitivePersistValue proxy o])
  cond' = renderCond' cond
  chains = projectionExprs p []
  RenderS fields fieldVals  = intercalateS (fromChar ',') $ concatMap (renderExprExtended escape 0) chains
  query = "SELECT " <> fields <> " FROM " <> tableName escape e constr <> whereClause <> orders <> lim
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  doSelectQuery = queryFunc query types binds $ mapAllRows $ liftM fst . projectionResult p
  binds = fieldVals . maybe id getValues cond' $ limps
  constr = constructors e !! entityConstrNum (undefined :: Proxy v) (undefined :: c a)
  types = map exprType chains where
    exprType (ExprRaw a) = dbType $ (undefined :: Expr db r a -> a) a
    exprType (ExprField ((_, t), _)) = t
    exprType (ExprPure a) = dbType a

count :: forall m db r v c . (db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c)
      => (Utf8 -> Utf8) -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> (Cond db r -> Maybe (RenderS db r)) -> Cond db r -> m Int
count escape queryFunc renderCond' cond = do
  let e = entityDef (undefined :: v)
      proxy = undefined :: Proxy (PhantomDb m)
      cond' = renderCond' cond
      constr = constructors e !! entityConstrNum (undefined :: Proxy v) (undefined :: c a)
      query = "SELECT COUNT(*) FROM " <> tableName escape e constr <> whereClause where
      whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  x <- queryFunc query [dbInt64] (maybe [] (flip getValues []) cond') id
  case x of
    Just [num] -> return $ fromPrimitivePersistValue proxy num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

replace :: forall m db r v . (PersistBackend m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
        => (Utf8 -> Utf8) -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> (Utf8 -> [PersistValue] -> m ()) -> (Bool -> Utf8 -> ConstructorDef -> [PersistValue] -> RenderS db r)
        -> Key v BackendSpecific -> v -> m ()
replace escape queryFunc execFunc insertIntoConstructorTable k v = do
  vals <- toEntityPersistValues' v
  let e = entityDef v
      proxy = undefined :: Proxy (PhantomDb m)
      constructorNum = fromPrimitivePersistValue proxy (head vals)
      constr = constructors e !! constructorNum

      upds = renderFields (\f -> escape f <> "=?") $ constrParams constr
      updateQuery = "UPDATE " <> tableName escape e constr <> " SET " <> upds <> " WHERE " <> fromString (fromJust $ constrAutoKeyName constr) <> "=?"

  if isSimple (constructors e)
    then execFunc updateQuery (tail vals ++ [toPrimitivePersistValue proxy k])
    else do
      let query = "SELECT discr FROM " <> mainTableName escape e <> " WHERE id=?"
      x <- queryFunc query [dbInt64] [toPrimitivePersistValue proxy k] (id >=> return . fmap (fromPrimitivePersistValue proxy . head))
      case x of
        Just discr -> do
          let cName = tableName escape e constr

          if discr == constructorNum
            then execFunc updateQuery (tail vals ++ [toPrimitivePersistValue proxy k])
            else do
              let RenderS insQuery vals' = insertIntoConstructorTable True cName constr (toPrimitivePersistValue proxy k:tail vals)
              execFunc insQuery (vals' [])

              let oldConstr = constructors e !! discr
              let delQuery = "DELETE FROM " <> tableName escape e oldConstr <> " WHERE " <> fromJust (constrId escape oldConstr) <> "=?"
              execFunc delQuery [toPrimitivePersistValue proxy k]

              let updateDiscrQuery = "UPDATE " <> mainTableName escape e <> " SET discr=? WHERE id=?"
              execFunc updateDiscrQuery [head vals, toPrimitivePersistValue proxy k]
        Nothing -> return ()

replaceBy :: forall m v u . (PersistBackend m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
          => (Utf8 -> Utf8) -- ^ escape
          -> (Utf8 -> [PersistValue] -> m ()) -- ^ function to execute query
          -> u (UniqueMarker v)
          -> v
          -> m ()
replaceBy escape execFunc u v = do
  uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
  vals <- toEntityPersistValues v
  let e = entityDef (undefined :: v)
      uFields = renderChain escape (fieldChain u) []
      RenderS cond condVals = intercalateS " AND " $ mkUniqueCond uFields uniques
      constr = head $ constructors e
      upds = renderFields (\f -> escape f <> "=?") $ constrParams constr
      updateQuery = "UPDATE " <> tableName escape e constr <> " SET " <> upds <> " WHERE " <> cond
  execFunc updateQuery (tail . vals . condVals $ [])

update :: forall m db r v c . (SqlDb db, QueryRaw db ~ Snippet db, db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c)
       => (Utf8 -> Utf8) -> (Utf8 -> [PersistValue] -> m ()) -> (Cond db r -> Maybe (RenderS db r)) -> [Update db r] -> Cond db r -> m ()
update escape execFunc renderCond' upds cond = do
  let e = entityDef (undefined :: v)
  case renderUpdates escape upds of
    Just upds' -> do
      let cond' = renderCond' cond
          constr = constructors e !! entityConstrNum (undefined :: Proxy v) (undefined :: c a)
          query = "UPDATE " <> tableName escape e constr <> " SET " <> whereClause where
          whereClause = maybe (getQuery upds') (\c -> getQuery upds' <> " WHERE " <> getQuery c) cond'
      execFunc query (getValues upds' <> maybe mempty getValues cond' $ [])
    Nothing -> return ()

delete :: forall m db r v c . (db ~ PhantomDb m, r ~ RestrictionHolder v c, PersistBackend m, PersistEntity v, EntityConstr v c)
       => (Utf8 -> Utf8) -> (Utf8 -> [PersistValue] -> m ()) -> (Cond db r -> Maybe (RenderS db r)) -> Cond db r -> m ()
delete escape execFunc renderCond' cond = execFunc query (maybe [] (($ []) . getValues) cond') where
  e = entityDef (undefined :: v)
  constr = constructors e !! entityConstrNum (undefined :: Proxy v) (undefined :: c a)
  cond' = renderCond' cond
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  query = if isSimple (constructors e)
    then "DELETE FROM " <> tableName escape e constr <> whereClause
    -- the entries in the constructor table are deleted because of the reference on delete cascade
    else "DELETE FROM " <> mainTableName escape e <> " WHERE id IN(SELECT " <> fromJust (constrId escape constr) <> " FROM " <> tableName escape e constr <> whereClause <> ")"

insertByAll :: forall m v . (PersistBackend m, PersistEntity v)
            => (Utf8 -> Utf8) -- ^ escape
            -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -- ^ function to run query
            -> Bool -- ^ allow multiple duplication of uniques with nulls
            -> v -> m (Either (AutoKey v) (AutoKey v))
insertByAll escape queryFunc manyNulls v = do
  let e = entityDef v
      proxy = undefined :: Proxy (PhantomDb m)
      (constructorNum, uniques) = getUniques proxy v
      constr = constructors e !! constructorNum
      uniqueDefs = constrUniques constr

      query = "SELECT " <> maybe "1" id (constrId escape constr) <> " FROM " <> tableName escape e constr <> " WHERE " <> cond
      conds = catMaybes $ zipWith (\u (_, uVals) -> checkNulls uVals $ intercalateS " AND " $ mkUniqueCond (f u) uVals) uniqueDefs uniques where
        f = foldr (flatten escape) [] . uniqueFields
      -- skip condition if any value is NULL. It allows to insert many values with duplicate unique key
      checkNulls uVals x = if manyNulls && any (== PersistNull) (uVals []) then Nothing else Just x
      RenderS cond vals = intercalateS " OR " conds
  if null conds
    then liftM Right $ Core.insert v
    else do
      x <- queryFunc query [dbInt64] (vals []) id
      case x of
        Nothing -> liftM Right $ Core.insert v
        Just xs -> return $ Left $ fst $ fromPurePersistValues proxy xs

deleteBy :: forall m v . (PersistBackend m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
            => (Utf8 -> Utf8) -> (Utf8 -> [PersistValue] -> m ()) -> Key v BackendSpecific -> m ()
deleteBy escape execFunc k = execFunc query [toPrimitivePersistValue proxy k] where
  e = entityDef ((undefined :: Key v u -> v) k)
  proxy = undefined :: Proxy (PhantomDb m)
  constr = head $ constructors e
  idName = if isSimple (constructors e)
    then fromJust $ constrId escape constr
    else "id"
  -- the entries in the constructor table are deleted because of the reference on delete cascade
  query = "DELETE FROM " <> mainTableName escape e <> " WHERE " <> idName <> "=?"

deleteAll :: forall m v . (PersistBackend m, PersistEntity v)
          => (Utf8 -> Utf8) -> (Utf8 -> [PersistValue] -> m ()) -> v -> m ()
deleteAll escape execFunc (_ :: v) = do
  let e = entityDef (undefined :: v)
      query = "DELETE FROM " <> mainTableName escape e
  execFunc query []

countAll :: forall m v . (PersistBackend m, PersistEntity v)
         => (Utf8 -> Utf8) -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a) -> v -> m Int
countAll escape queryFunc (_ :: v) = do
  let e = entityDef (undefined :: v)
      proxy = undefined :: Proxy (PhantomDb m)
      query = "SELECT COUNT(*) FROM " <> mainTableName escape e
  x <- queryFunc query [dbInt64] [] id
  case x of
    Just [num] -> return $ fromPrimitivePersistValue proxy num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

insertBy :: forall m v u . (PersistBackend m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
         => (Utf8 -> Utf8)
         -> (forall a . Utf8 -> [DbType] -> [PersistValue] -> (RowPopper m -> m a) -> m a)
         -> Bool
         -> u (UniqueMarker v) -> v -> m (Either (AutoKey v) (AutoKey v))
insertBy escape queryFunc manyNulls u v = do
  uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
  let e = entityDef v
      proxy = undefined :: Proxy (PhantomDb m)
      uFields = renderChain escape (fieldChain u) []
      RenderS cond vals = intercalateS " AND " $ mkUniqueCond uFields uniques
      -- skip condition if any value is NULL. It allows to insert many values with duplicate unique key
      checkNulls uVals = manyNulls && any (== PersistNull) (uVals [])
      -- this is safe because unique keys exist only for entities with one constructor
      constr = head $ constructors e
      query = "SELECT " <> maybe "1" id (constrId escape constr) <> " FROM " <> tableName escape e constr <> " WHERE " <> cond
  if checkNulls uniques
    then liftM Right $ Core.insert v
    else do
      x <- queryFunc query [dbInt64] (vals []) id
      case x of
        Nothing  -> liftM Right $ Core.insert v
        Just [k] -> return $ Left $ fst $ fromPurePersistValues proxy [k]
        Just xs  -> fail $ "unexpected query result: " ++ show xs

getConstructorTypes :: ConstructorDef -> [DbType]
getConstructorTypes = map snd . constrParams where

constrId :: (Utf8 -> Utf8) -> ConstructorDef -> Maybe Utf8
constrId escape = fmap (escape . fromString) . constrAutoKeyName

-- | receives constructor number and row of values from the constructor table
mkEntity :: (PersistEntity v, PersistBackend m) => Proxy (PhantomDb m) -> Int -> [PersistValue] -> m (AutoKey v, v)
mkEntity proxy i xs = fromEntityPersistValues (toPrimitivePersistValue proxy i:xs') >>= \(v, _) -> return (k, v) where
  (k, xs') = fromPurePersistValues proxy xs

toEntityPersistValues' :: (PersistBackend m, PersistEntity v) => v -> m [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

dbInt64 :: DbType
dbInt64 = DbTypePrimitive DbInt64 False Nothing Nothing

mkUniqueCond :: [Utf8] -> ([PersistValue] -> [PersistValue]) -> [RenderS db r]
mkUniqueCond u vals = zipWith f u (vals []) where
  f a PersistNull = RenderS (a <> " IS NULL") id
  f a x = RenderS (a <> "=?") (x:)
