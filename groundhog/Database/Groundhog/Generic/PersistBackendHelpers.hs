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

import Database.Groundhog.Core hiding (PersistBackendConn(..))
import Database.Groundhog.Core (PersistBackendConn)
import qualified Database.Groundhog.Core as Core
import Database.Groundhog.Generic (firstRow, mapStream, streamToList, joinStreams, isSimple, getUniqueFields)
import Database.Groundhog.Generic.Sql

import Control.Monad (liftM)
import Data.Either (rights)
import Data.Maybe (catMaybes, fromJust, mapMaybe)

get :: forall conn v . (PersistBackendConn conn, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
    => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -> Key v BackendSpecific -> Action conn (Maybe v)
get RenderConfig{..} queryFunc (k :: Key v BackendSpecific) = do
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy conn
  if isSimple (constructors e)
    then do
      let constr = head $ constructors e
      let fields = renderFields esc (constrParams constr)
      let query = "SELECT " <> fields <> " FROM " <> tableName esc e constr <> " WHERE " <> fromJust (constrId esc constr) <> "=?"
      x <- queryFunc query [toPrimitivePersistValue k] >>= firstRow
      case x of
        Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
        Nothing -> return Nothing
    else do
      let query = "SELECT discr FROM " <> mainTableName esc e <> " WHERE id=?"
      x <- queryFunc query [toPrimitivePersistValue k] >>= firstRow
      case x of
        Just [discr] -> do
          let constructorNum = fromPrimitivePersistValue discr
              constr = constructors e !! constructorNum
              fields = renderFields esc (constrParams constr)
              cQuery = "SELECT " <> fields <> " FROM " <> tableName esc e constr <> " WHERE " <> fromJust (constrId esc constr) <> "=?"
          x2 <- queryFunc cQuery [toPrimitivePersistValue k] >>= firstRow
          case x2 of
            Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ discr:xs
            Nothing -> fail "Missing entry in constructor table"
        Just x' -> fail $ "Unexpected number of columns returned: " ++ show x'
        Nothing -> return Nothing

select :: forall conn r v c opts . (SqlDb conn, r ~ RestrictionHolder v c, PersistBackendConn conn, PersistEntity v, EntityConstr v c, HasSelectOptions opts conn r)
       => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -> (opts -> RenderS conn r) -> Utf8 -> opts -> Action conn [v]
select conf queryFunc preColumns noLimit options = selectStream conf queryFunc preColumns noLimit options >>= streamToList

selectStream :: forall conn r v c opts . (SqlDb conn, r ~ RestrictionHolder v c, PersistBackendConn conn, PersistEntity v, EntityConstr v c, HasSelectOptions opts conn r)
       => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -> (opts -> RenderS conn r) -> Utf8 -> opts -> Action conn (RowStream v)
selectStream conf@RenderConfig{..} queryFunc preColumns noLimit options = doSelectQuery where
  SelectOptions cond limit offset ords dist _ = getSelectOptions options

  e = entityDef proxy (undefined :: v)
  proxy = undefined :: proxy conn
  orders = renderOrders conf ords
  lim = case (limit, offset) of
          (Nothing, Nothing) -> mempty
          (Nothing, o) -> RenderS (" " <> noLimit <> " OFFSET ?") (toPurePersistValues o)
          (l, Nothing) -> RenderS " LIMIT ?" (toPurePersistValues l)
          (l, o) -> RenderS " LIMIT ? OFFSET ?" (toPurePersistValues (l, o))
  cond' = renderCond conf cond
  fields = RenderS (renderFields esc (constrParams constr)) id
  distinctClause = if dist then "DISTINCT " else mempty
  RenderS query binds = "SELECT " <> distinctClause <> preColumns options <> fields <> " FROM " <> RenderS (tableName esc e constr) id <> whereClause <> orders <> lim
  whereClause = maybe "" (" WHERE " <>) cond'
  doSelectQuery = queryFunc query (binds []) >>= mapStream (\xs -> liftM fst $ fromEntityPersistValues $ (toPrimitivePersistValue cNum):xs)
  cNum = entityConstrNum (undefined :: proxy v) (undefined :: c a)
  constr = constructors e !! cNum

selectAll :: forall conn v . (PersistBackendConn conn, PersistEntity v)
          => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -> Action conn [(AutoKey v, v)]
selectAll conf queryFunc = selectAllStream conf queryFunc >>= streamToList

-- | It may call the passed function multiple times
selectAllStream :: forall conn v . (PersistBackendConn conn, PersistEntity v)
                => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -> Action conn (RowStream (AutoKey v, v))
selectAllStream RenderConfig{..} queryFunc = start where
  start = joinStreams $ zipWith selectConstr [0..] $ constructors e
  selectConstr cNum constr = queryFunc query [] >>= mapStream (mkEntity cNum) where
    fields = maybe id (\key cont -> key <> fromChar ',' <> cont) (constrId esc constr) $ renderFields esc (constrParams constr)
    query = "SELECT " <> fields <> " FROM " <> tableName esc e constr
  e = entityDef proxy (undefined :: v)
  proxy = undefined :: proxy conn
  mkEntity cNum xs = do
    let (k, xs') = fromPurePersistValues xs
    (v, _) <- fromEntityPersistValues (toPrimitivePersistValue (cNum :: Int):xs')
    return (k, v)

getBy :: forall conn v u . (PersistBackendConn conn, PersistEntity v, IsUniqueKey (Key v (Unique u)))
      => RenderConfig
      -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -- ^ function to run query
      -> Key v (Unique u)
      -> Action conn (Maybe v)
getBy conf@RenderConfig{..} queryFunc (k :: Key v (Unique u)) = do
  uniques <- toPersistValues k
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy conn
      u = (undefined :: Key v (Unique u) -> u (UniqueMarker v)) k
      uFields = renderChain conf (fieldChain proxy u) []
      RenderS cond vals = intercalateS " AND " $ mkUniqueCond uFields uniques
      constr = head $ constructors e
      fields = renderFields esc (constrParams constr)
      query = "SELECT " <> fields <> " FROM " <> tableName esc e constr <> " WHERE " <> cond
  x <- queryFunc query (vals []) >>= firstRow
  case x of
    Just xs -> liftM (Just . fst) $ fromEntityPersistValues $ PersistInt64 0:xs
    Nothing -> return Nothing

project :: forall conn r v c p opts a'. (SqlDb conn, r ~ RestrictionHolder v c, PersistBackendConn conn, PersistEntity v, EntityConstr v c, Projection p a', ProjectionDb p conn, ProjectionRestriction p r, HasSelectOptions opts conn r)
        => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -> (opts -> RenderS conn r) -> Utf8 -> p -> opts -> Action conn [a']
project conf queryFunc preColumns noLimit p options = projectStream conf queryFunc preColumns noLimit p options >>= streamToList

projectStream :: forall conn r v c p opts a' . (SqlDb conn, r ~ RestrictionHolder v c, PersistBackendConn conn, PersistEntity v, EntityConstr v c, Projection p a', ProjectionDb p conn, ProjectionRestriction p r, HasSelectOptions opts conn r)
        => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -> (opts -> RenderS conn r) -> Utf8 -> p -> opts -> Action conn (RowStream a')
projectStream conf@RenderConfig{..} queryFunc preColumns noLimit p options = doSelectQuery where
  SelectOptions cond limit offset ords dist _ = getSelectOptions options
  e = entityDef proxy (undefined :: v)
  proxy = undefined :: proxy conn
  orders = renderOrders conf ords
  lim = case (limit, offset) of
          (Nothing, Nothing) -> mempty
          (Nothing, o) -> RenderS (" " <> noLimit <> " OFFSET ?") (toPurePersistValues o)
          (l, Nothing) -> RenderS " LIMIT ?" (toPurePersistValues l)
          (l, o) -> RenderS " LIMIT ? OFFSET ?" (toPurePersistValues (l, o))
  cond' = renderCond conf cond
  chains = projectionExprs p []
  fields = commasJoin $ concatMap (renderExprExtended conf 0) chains
  distinctClause = if dist then "DISTINCT " else mempty
  RenderS query binds = "SELECT " <> distinctClause <> preColumns options <> fields <> " FROM " <> RenderS (tableName esc e constr) id <> whereClause <> orders <> lim
  whereClause = maybe "" (" WHERE " <>) cond'
  doSelectQuery = queryFunc query (binds []) >>= mapStream (liftM fst . projectionResult p)
  constr = constructors e !! entityConstrNum (undefined :: proxy v) (undefined :: c a)

count :: forall conn r v c . (SqlDb conn, r ~ RestrictionHolder v c, PersistBackendConn conn, PersistEntity v, EntityConstr v c)
      => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -> Cond conn r -> Action conn Int
count conf@RenderConfig{..} queryFunc cond = do
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy conn
      cond' = renderCond conf cond
      constr = constructors e !! entityConstrNum (undefined :: proxy v) (undefined :: c a)
      query = "SELECT COUNT(*) FROM " <> tableName esc e constr <> whereClause where
      whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  x <- queryFunc query (maybe [] (flip getValues []) cond') >>= firstRow
  case x of
    Just [num] -> return $ fromPrimitivePersistValue num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

replace :: forall conn r v . (PersistBackendConn conn, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
        => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue]))
        -> (Utf8 -> [PersistValue] -> Action conn ())
        -> (Bool -> Utf8 -> ConstructorDef -> [PersistValue] -> RenderS conn r)
        -> Key v BackendSpecific -> v -> Action conn ()
replace RenderConfig{..} queryFunc execFunc insertIntoConstructorTable k v = do
  vals <- toEntityPersistValues' v
  let e = entityDef proxy v
      proxy = undefined :: proxy conn
      constructorNum = fromPrimitivePersistValue (head vals)
      constr = constructors e !! constructorNum
      k' = toPrimitivePersistValue k
      RenderS upds updsVals = commasJoin $ zipWith f fields $ tail vals where
        fields = foldr (flatten esc) [] $ constrParams constr
        f f1 f2 = RenderS f1 id <> fromChar '=' <> renderPersistValue f2
      updateQuery = "UPDATE " <> tableName esc e constr <> " SET " <> upds <> " WHERE " <> fromString (fromJust $ constrAutoKeyName constr) <> "=?"

  if isSimple (constructors e)
    then execFunc updateQuery (updsVals [k'])
    else do
      let query = "SELECT discr FROM " <> mainTableName esc e <> " WHERE id=?"
      x <- queryFunc query [k'] >>= liftM (fmap $ fromPrimitivePersistValue . head) . firstRow
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

replaceBy :: forall conn v u . (PersistBackendConn conn, PersistEntity v, IsUniqueKey (Key v (Unique u)))
          => RenderConfig
          -> (Utf8 -> [PersistValue] -> Action conn ()) -- ^ function to execute query
          -> u (UniqueMarker v)
          -> v
          -> Action conn ()
replaceBy conf@RenderConfig{..} execFunc u v = do
  uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
  vals <- toEntityPersistValues' v
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy conn
      uFields = renderChain conf (fieldChain proxy u) []
      RenderS cond condVals = intercalateS " AND " $ mkUniqueCond uFields uniques
      constr = head $ constructors e
      RenderS upds updsVals = commasJoin $ zipWith f fields $ tail vals where
        fields = foldr (flatten esc) [] $ constrParams constr
        f f1 f2 = RenderS f1 id <> fromChar '=' <> renderPersistValue f2
      updateQuery = "UPDATE " <> tableName esc e constr <> " SET " <> upds <> " WHERE " <> cond
  execFunc updateQuery (updsVals . condVals $ [])

update :: forall conn r v c . (SqlDb conn, r ~ RestrictionHolder v c, PersistBackendConn conn, PersistEntity v, EntityConstr v c)
       => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn ()) -> [Update conn r] -> Cond conn r -> Action conn ()
update conf@RenderConfig{..} execFunc upds cond = do
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy conn
  case renderUpdates conf upds of
    Just upds' -> do
      let cond' = renderCond conf cond
          constr = constructors e !! entityConstrNum (undefined :: proxy v) (undefined :: c a)
          query = "UPDATE " <> tableName esc e constr <> " SET " <> whereClause where
          whereClause = maybe (getQuery upds') (\c -> getQuery upds' <> " WHERE " <> getQuery c) cond'
      execFunc query (getValues upds' <> maybe mempty getValues cond' $ [])
    Nothing -> return ()

delete :: forall conn r v c . (SqlDb conn, r ~ RestrictionHolder v c, PersistBackendConn conn, PersistEntity v, EntityConstr v c)
       => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn ()) -> Cond conn r -> Action conn ()
delete conf@RenderConfig{..} execFunc cond = execFunc query (maybe [] (($ []) . getValues) cond') where
  e = entityDef proxy (undefined :: v)
  proxy = undefined :: proxy conn
  constr = constructors e !! entityConstrNum (undefined :: proxy v) (undefined :: c a)
  cond' = renderCond conf cond
  whereClause = maybe "" (\c -> " WHERE " <> getQuery c) cond'
  query = if isSimple (constructors e)
    then "DELETE FROM " <> tableName esc e constr <> whereClause
    -- the entries in the constructor table are deleted because of the reference on delete cascade
    else "DELETE FROM " <> mainTableName esc e <> " WHERE id IN(SELECT " <> fromJust (constrId esc constr) <> " FROM " <> tableName esc e constr <> whereClause <> ")"

insertByAll :: forall conn v . (PersistBackendConn conn, PersistEntity v)
            => RenderConfig
            -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -- ^ function to run query
            -> Bool -- ^ allow multiple duplication of uniques with nulls
            -> v -> Action conn (Either (AutoKey v) (AutoKey v))
insertByAll RenderConfig{..} queryFunc manyNulls v = do
  let e = entityDef proxy v
      proxy = undefined :: proxy conn
      (constructorNum, uniques) = getUniques v
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
      x <- queryFunc query (vals []) >>= firstRow
      case x of
        Nothing -> liftM Right $ Core.insert v
        Just xs -> return $ Left $ fst $ fromPurePersistValues xs

deleteBy :: forall conn v . (PersistBackendConn conn, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
            => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn ()) -> Key v BackendSpecific -> Action conn ()
deleteBy RenderConfig{..} execFunc k = execFunc query [toPrimitivePersistValue k] where
  e = entityDef proxy ((undefined :: Key v u -> v) k)
  proxy = undefined :: proxy conn
  constr = head $ constructors e
  idName = if isSimple (constructors e)
    then fromJust $ constrId esc constr
    else "id"
  -- the entries in the constructor table are deleted because of the reference on delete cascade
  query = "DELETE FROM " <> mainTableName esc e <> " WHERE " <> idName <> "=?"

deleteAll :: forall conn v . (PersistBackendConn conn, PersistEntity v)
          => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn ()) -> v -> Action conn ()
deleteAll RenderConfig{..} execFunc (_ :: v) = execFunc query [] where
  e = entityDef proxy (undefined :: v)
  proxy = undefined :: proxy conn
  query = "DELETE FROM " <> mainTableName esc e

countAll :: forall conn v . (PersistBackendConn conn, PersistEntity v)
         => RenderConfig -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue])) -> v -> Action conn Int
countAll RenderConfig{..} queryFunc (_ :: v) = do
  let e = entityDef proxy (undefined :: v)
      proxy = undefined :: proxy conn
      query = "SELECT COUNT(*) FROM " <> mainTableName esc e
  x <- queryFunc query [] >>= firstRow
  case x of
    Just [num] -> return $ fromPrimitivePersistValue num
    Just xs -> fail $ "requested 1 column, returned " ++ show (length xs)
    Nothing -> fail $ "COUNT returned no rows"

insertBy :: forall conn v u . (PersistBackendConn conn, PersistEntity v, IsUniqueKey (Key v (Unique u)))
         => RenderConfig
         -> (Utf8 -> [PersistValue] -> Action conn (RowStream [PersistValue]))
         -> Bool
         -> u (UniqueMarker v) -> v -> Action conn (Either (AutoKey v) (AutoKey v))
insertBy conf@RenderConfig{..} queryFunc manyNulls u v = do
  uniques <- toPersistValues $ (extractUnique v `asTypeOf` ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u))
  let e = entityDef proxy v
      proxy = undefined :: proxy conn
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
      x <- queryFunc query (vals []) >>= firstRow
      case x of
        Nothing  -> liftM Right $ Core.insert v
        Just [k] -> return $ Left $ fst $ fromPurePersistValues [k]
        Just xs  -> fail $ "unexpected query result: " ++ show xs

constrId :: (Utf8 -> Utf8) -> ConstructorDef -> Maybe Utf8
constrId escape = fmap (escape . fromString) . constrAutoKeyName

toEntityPersistValues' :: (PersistBackendConn conn, PersistEntity v) => v -> Action conn [PersistValue]
toEntityPersistValues' = liftM ($ []) . toEntityPersistValues

mkUniqueCond :: [Utf8] -> ([PersistValue] -> [PersistValue]) -> [RenderS conn r]
mkUniqueCond u vals = zipWith f u (vals []) where
  f a PersistNull = RenderS (a <> " IS NULL") id
  f a x = RenderS (a <> "=?") (x:)
