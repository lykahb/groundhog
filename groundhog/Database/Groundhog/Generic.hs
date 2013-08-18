{-# LANGUAGE FlexibleContexts, ExistentialQuantification, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}

-- | This helper module is intended for use by the backend creators
module Database.Groundhog.Generic
  ( 
  -- * Migration
    createMigration
  , executeMigration
  , executeMigrationUnsafe
  , runMigration
  , runMigrationUnsafe
  , printMigration
  , mergeMigrations
  , silentMigrationLogger
  , defaultMigrationLogger
  , failMessage
  -- * Helpers for running Groundhog within custom monads
  , HasConn
  , runDb
  , withSavepoint
  -- * Helper functions for defining *PersistValue instances
  , primToPersistValue
  , primFromPersistValue
  , primToPurePersistValues
  , primFromPurePersistValues
  , primToSinglePersistValue
  , primFromSinglePersistValue
  , pureToPersistValue
  , pureFromPersistValue
  , singleToPersistValue
  , singleFromPersistValue
  , toSinglePersistValueUnique
  , fromSinglePersistValueUnique
  , toPersistValuesUnique
  , fromPersistValuesUnique
  , toSinglePersistValueAutoKey
  , fromSinglePersistValueAutoKey
  -- * Other
  , bracket
  , finally
  , onException
  , PSFieldDef(..)
  , applyDbTypeSettings
  , findOne
  , replaceOne
  , matchElements
  , haveSameElems
  , mapAllRows
  , phantomDb
  , isSimple
  ) where

import Database.Groundhog.Core

import Control.Applicative ((<|>))
import Control.Monad (liftM, forM_, (>=>))
import Control.Monad.Trans.State (StateT (..))
import Control.Monad.Trans.Control (MonadBaseControl, control, restoreM)
import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader(..))
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (partition, sortBy)
import qualified Data.Map as Map

getCorrectMigrations :: NamedMigrations -> [(Bool, Int, String)]
getCorrectMigrations = either (error.unlines) id . mergeMigrations . Map.elems

-- | Produce the migrations but not execute them. Fails when an unsafe migration occurs.
createMigration :: PersistBackend m => Migration m -> m NamedMigrations
createMigration m = liftM snd $ runStateT m Map.empty

-- | Execute the migrations and log them. 
executeMigration :: (PersistBackend m, MonadIO m) => (String -> IO ()) -> NamedMigrations -> m ()
executeMigration logger m = do
  let migs = getCorrectMigrations m
  let unsafe = filter (\(isUnsafe, _, _) -> isUnsafe) migs
  if null unsafe
    then mapM_ (\(_, _, query) -> executeMigrate logger query) $ sortBy (compare `on` \(_, i, _) -> i) migs
    else error $ concat
            [ "\n\nDatabase migration: manual intervention required.\n"
            , "The following actions are considered unsafe:\n\n"
            , unlines $ map (\(_, _, query) -> "    " ++ query ++ ";") unsafe
            ]

-- | Execute migrations and log them. Executes the unsafe migrations without warnings
executeMigrationUnsafe :: (PersistBackend m, MonadIO m) => (String -> IO ()) -> NamedMigrations -> m ()
executeMigrationUnsafe logger = mapM_ (\(_, _, query) -> executeMigrate logger query) . getCorrectMigrations

-- | Pretty print the migrations
printMigration :: MonadIO m => NamedMigrations -> m ()
printMigration migs = liftIO $ do
  let kv = Map.assocs migs
  forM_ kv $ \(k, v) -> do
    putStrLn $ "Datatype " ++ k ++ ":"
    case v of
      Left errors -> mapM_ (putStrLn . ("\tError:\t" ++)) errors
      Right sqls  -> do
        let showSql (isUnsafe, _, sql) = (if isUnsafe then "Unsafe:\t" else "Safe:\t") ++ sql
        mapM_ (putStrLn . ("\t" ++) . showSql) sqls

-- | Run migrations and log them. Fails when an unsafe migration occurs.
runMigration :: (PersistBackend m, MonadIO m) => (String -> IO ()) -> Migration m -> m ()
runMigration logger m = createMigration m >>= executeMigration logger

-- | Run migrations and log them. Executes the unsafe migrations without warnings
runMigrationUnsafe :: (PersistBackend m, MonadIO m) => (String -> IO ()) -> Migration m -> m ()
runMigrationUnsafe logger m = createMigration m >>= executeMigrationUnsafe logger

executeMigrate :: (PersistBackend m, MonadIO m) => (String -> IO ()) -> String -> m ()
executeMigrate logger query = do
  liftIO $ logger query
  executeRaw False query []
  return ()

-- | No-op
silentMigrationLogger :: String -> IO ()
silentMigrationLogger _ = return ()

-- | Prints the queries to stdout
defaultMigrationLogger :: String -> IO ()
defaultMigrationLogger query = putStrLn $ "Migrating: " ++ query

-- | Joins the migrations. The result is either all error messages or all queries
mergeMigrations :: [SingleMigration] -> SingleMigration
mergeMigrations ms =
  let (errors, statements) = partitionEithers ms
  in if null errors
       then Right (concat statements)
       else Left  (concat errors)

failMessage :: PersistField a => a -> [PersistValue] -> String
failMessage a xs = "Invalid list for " ++ persistName a ++ ": " ++ show xs

finally :: MonadBaseControl IO m
        => m a -- ^ computation to run first
        -> m b -- ^ computation to run afterward (even if an exception was raised)
        -> m a
finally a sequel = control $ \runInIO ->
                     E.finally (runInIO a)
                               (runInIO sequel)

bracket :: MonadBaseControl IO m
        => m a        -- ^ computation to run first ("acquire resource")
        -> (a -> m b) -- ^ computation to run last ("release resource")
        -> (a -> m c) -- ^ computation to run in-between
        -> m c
bracket before after thing = control $ \runInIO ->
                     E.bracket (runInIO before) (\st -> runInIO $ restoreM st >>= after) (\st -> runInIO $ restoreM st >>= thing)

onException :: MonadBaseControl IO m
        => m a
        -> m b
        -> m a
onException io what = control $ \runInIO -> E.onException (runInIO io) (runInIO what)

data PSFieldDef = PSFieldDef {
    psFieldName :: String -- bar
  , psDbFieldName :: Maybe String -- SQLbar
  , psDbTypeName :: Maybe String -- inet, NUMERIC(5,2), VARCHAR(50)
  , psExprName :: Maybe String -- BarField
  , psEmbeddedDef :: Maybe [PSFieldDef]
  , psDefaultValue :: Maybe String
  , psReferenceParent :: Maybe (Maybe String, String, [String])
  , psReferenceOnDelete :: Maybe ReferenceActionType
  , psReferenceOnUpdate :: Maybe ReferenceActionType
} deriving Show

applyDbTypeSettings :: PSFieldDef -> DbType -> DbType
applyDbTypeSettings f@(PSFieldDef _ _ dbTypeName _ Nothing def _ _ _) typ = case typ of
  DbTypePrimitive t nullable def' ref -> DbTypePrimitive (maybe t (DbOther . OtherTypeDef . const) dbTypeName) nullable (def <|> def') (applyReferencesSettings f ref)
  DbEmbedded emb ref -> DbEmbedded emb (applyReferencesSettings f ref)
  t -> t
applyDbTypeSettings f@(PSFieldDef _ _ _ _ (Just subs) _ _ _ _) typ = (case typ of
  DbEmbedded (EmbeddedDef _ fields) ref -> DbEmbedded (uncurry EmbeddedDef $ go subs fields) (applyReferencesSettings f ref)
  t -> error $ "applyDbTypeSettings: expected DbEmbedded, got " ++ show t) where
  go [] fs = (False, fs)
  go st [] = error $ "applyDbTypeSettings: embedded datatype does not have expected fields: " ++ show st
  go st (field@(fName, fType):fs) = case partition ((== fName) . psFieldName) st of
    ([fDef], rest) -> result where
      (flag, fields') = go rest fs
      result = case psDbFieldName fDef of
        Nothing -> (flag, (fName, applyDbTypeSettings fDef fType):fields')
        Just name' -> (True, (name', applyDbTypeSettings fDef fType):fields')
    _ -> let (flag, fields') = go st fs in (flag, field:fields')

applyReferencesSettings :: PSFieldDef -> Maybe ParentTableReference -> Maybe ParentTableReference
applyReferencesSettings (PSFieldDef _ _ _ _ _ _ Nothing Nothing Nothing) ref = ref
applyReferencesSettings (PSFieldDef _ _ _ _ _ _ parent onDel onUpd) (Just (parent', onDel', onUpd')) = Just (maybe parent' Right parent, onDel <|> onDel', onUpd <|> onUpd')
applyReferencesSettings (PSFieldDef _ _ _ _ _ _ (Just parent) onDel onUpd) Nothing = Just (Right parent, onDel, onUpd)
applyReferencesSettings _ Nothing = error $ "applyReferencesSettings: expected type with reference, got Nothing"

primToPersistValue :: (PersistBackend m, PrimitivePersistField a) => a -> m ([PersistValue] -> [PersistValue])
primToPersistValue a = phantomDb >>= \p -> return (toPrimitivePersistValue p a:)

primFromPersistValue :: (PersistBackend m, PrimitivePersistField a) => [PersistValue] -> m (a, [PersistValue])
primFromPersistValue (x:xs) = phantomDb >>= \p -> return (fromPrimitivePersistValue p x, xs)
primFromPersistValue xs = (\a -> fail (failMessage a xs) >> return (a, xs)) undefined

primToPurePersistValues :: (DbDescriptor db, PrimitivePersistField a) => Proxy db -> a -> ([PersistValue] -> [PersistValue])
primToPurePersistValues p a = (toPrimitivePersistValue p a:)

primFromPurePersistValues :: (DbDescriptor db, PrimitivePersistField a) => Proxy db -> [PersistValue] -> (a, [PersistValue])
primFromPurePersistValues p (x:xs) = (fromPrimitivePersistValue p x, xs)
primFromPurePersistValues _ xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

primToSinglePersistValue :: (PersistBackend m, PrimitivePersistField a) => a -> m PersistValue
primToSinglePersistValue a = phantomDb >>= \p -> return (toPrimitivePersistValue p a)

primFromSinglePersistValue :: (PersistBackend m, PrimitivePersistField a) => PersistValue -> m a
primFromSinglePersistValue a = phantomDb >>= \p -> return (fromPrimitivePersistValue p a)

pureToPersistValue :: (PersistBackend m, PurePersistField a) => a -> m ([PersistValue] -> [PersistValue])
pureToPersistValue a = phantomDb >>= \p -> return (toPurePersistValues p a)

pureFromPersistValue :: (PersistBackend m, PurePersistField a) => [PersistValue] -> m (a, [PersistValue])
pureFromPersistValue xs = phantomDb >>= \p -> return (fromPurePersistValues p xs)

singleToPersistValue :: (PersistBackend m, SinglePersistField a) => a -> m ([PersistValue] -> [PersistValue])
singleToPersistValue a = toSinglePersistValue a >>= \x -> return (x:)

singleFromPersistValue :: (PersistBackend m, SinglePersistField a) => [PersistValue] -> m (a, [PersistValue])
singleFromPersistValue (x:xs) = fromSinglePersistValue x >>= \a -> return (a, xs)
singleFromPersistValue xs = (\a -> fail (failMessage a xs) >> return (a, xs)) undefined

toSinglePersistValueUnique :: forall m v u . (PersistBackend m, PersistEntity v, IsUniqueKey (Key v (Unique u)), PrimitivePersistField (Key v (Unique u)))
                           => u (UniqueMarker v) -> v -> m PersistValue
toSinglePersistValueUnique u v = insertBy u v >> primToSinglePersistValue (extractUnique v :: Key v (Unique u))

fromSinglePersistValueUnique :: forall m v u . (PersistBackend m, PersistEntity v, IsUniqueKey (Key v (Unique u)), PrimitivePersistField (Key v (Unique u)))
                             => u (UniqueMarker v) -> PersistValue -> m v
fromSinglePersistValueUnique _ x = phantomDb >>= \proxy -> getBy (fromPrimitivePersistValue proxy x :: Key v (Unique u)) >>= maybe (fail $ "No data with id " ++ show x) return

toPersistValuesUnique :: forall m v u . (PersistBackend m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
                      => u (UniqueMarker v) -> v -> m ([PersistValue] -> [PersistValue])
toPersistValuesUnique u v = insertBy u v >> toPersistValues (extractUnique v :: Key v (Unique u))

fromPersistValuesUnique :: forall m v u . (PersistBackend m, PersistEntity v, IsUniqueKey (Key v (Unique u)))
                        => u (UniqueMarker v) -> [PersistValue] -> m (v, [PersistValue])
fromPersistValuesUnique _ xs = fromPersistValues xs >>= \(k, xs') -> getBy (k :: Key v (Unique u)) >>= maybe (fail $ "No data with id " ++ show xs) (\v -> return (v, xs'))

toSinglePersistValueAutoKey :: forall m v . (PersistBackend m, PersistEntity v, PrimitivePersistField (AutoKey v))
                            => v -> m PersistValue
toSinglePersistValueAutoKey a = insertByAll a >>= primToSinglePersistValue . either id id

fromSinglePersistValueAutoKey :: forall m v . (PersistBackend m, PersistEntity v, PrimitivePersistField (Key v BackendSpecific))
                              => PersistValue -> m v
fromSinglePersistValueAutoKey x = phantomDb >>= \p -> get (fromPrimitivePersistValue p x :: Key v BackendSpecific) >>= maybe (fail $ "No data with id " ++ show x) return

replaceOne :: (Eq c, Show c) => String -> (a -> c) -> (b -> c) -> (a -> b -> b) -> a -> [b] -> [b]
replaceOne what getter1 getter2 apply a bs = case length (filter ((getter1 a ==) . getter2) bs) of
  1 -> map (\b -> if getter1 a == getter2 b then apply a b else b) bs
  0 -> error $ "Not found " ++ what ++ " with name " ++ show (getter1 a)
  _ -> error $ "Found more than one " ++ what ++ " with name " ++ show (getter1 a)

findOne :: (Eq c, Show c) => String -> (a -> c) -> (b -> c) -> a -> [b] -> b
findOne what getter1 getter2 a bs = case filter ((getter1 a ==) . getter2) bs of
  [b] -> b
  []  -> error $ "Not found " ++ what ++ " with name " ++ show (getter1 a)
  _   -> error $ "Found more than one " ++ what ++ " with name " ++ show (getter1 a)

-- | Returns only old elements, only new elements, and matched pairs (old, new).
-- The new ones exist only in datatype, the old are present only in DB, match is typically by name (the properties of the matched elements may differ).
matchElements :: Show a => (a -> b -> Bool) -> [a] -> [b] -> ([a], [b], [(a, b)])
matchElements eq oldElems newElems = foldr f (oldElems, [], []) newElems where
  f new (olds, news, matches) = case partition (`eq` new) olds of
    ([], rest) -> (rest, new:news, matches)
    ([old], rest) -> (rest, news, (old, new):matches)
    (xs, _) -> error $ "matchElements: more than one element matched " ++ show xs

haveSameElems :: Show a => (a -> b -> Bool) -> [a] -> [b] -> Bool
haveSameElems p xs ys = case matchElements p xs ys of
  ([], [], _) -> True
  _           -> False

mapAllRows :: Monad m => ([PersistValue] -> m a) -> RowPopper m -> m [a]
mapAllRows f pop = go where
  go = pop >>= maybe (return []) (f >=> \a -> liftM (a:) go)

phantomDb :: PersistBackend m => m (Proxy (PhantomDb m))
phantomDb = return $ error "phantomDb"

isSimple :: [ConstructorDef] -> Bool
isSimple [_] = True
isSimple _   = False

-- | This class helps to shorten the type signatures of user monadic code.
class (MonadIO m, MonadBaseControl IO m, MonadReader cm m, ConnectionManager cm conn) => HasConn m cm conn
instance (MonadIO m, MonadBaseControl IO m, MonadReader cm m, ConnectionManager cm conn) => HasConn m cm conn

-- | It helps to run database operations within your application monad.
runDb :: HasConn m cm conn => DbPersist conn IO a -> m a
runDb f = ask >>= liftIO . runDbConn f

-- | It helps to run 'withConnSavepoint' within a monad.
withSavepoint :: (HasConn m cm conn, SingleConnectionManager cm conn, Savepoint conn) => String -> m a -> m a
withSavepoint name m = ask >>= withConnNoTransaction (withConnSavepoint name m)
