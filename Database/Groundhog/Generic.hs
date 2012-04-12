{-# LANGUAGE FlexibleContexts #-}

-- | This helper module is intended for use by the backend creators
module Database.Groundhog.Generic
  ( migrateRecursively
  , createMigration
  , executeMigration
  , executeMigrationUnsafe
  , runMigration
  , runMigrationUnsafe
  , printMigration
  , mergeMigrations
  , silentMigrationLogger
  , defaultMigrationLogger
  , failMessage
  , Column(..)
  , mkColumns
  , bracket
  , finally
  , onException
  , PSEmbeddedFieldDef(..)
  , applyEmbeddedDbTypeSettings
  ) where

import Database.Groundhog.Core

import Control.Monad (liftM, forM_)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl, control, restoreM)
import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map

-- | Create migration for a given entity and all entities it depends on.
-- The stateful Map is used to avoid duplicate migrations when an entity type
-- occurs several times in a datatype
migrateRecursively :: (Monad m, PersistEntity e) => 
     (EntityDef -> m SingleMigration) -- ^ migrate entity
  -> (DbType    -> m SingleMigration) -- ^ migrate list
  -> e                                -- ^ initial entity
  -> StateT NamedMigrations m ()
migrateRecursively migE migL = go . dbType where
  go l@(DbList lName t) = f lName (migL l) (go t)
  go (DbEntity e)       = f (entityName e) (migE e) (mapM_ go (allSubtypes e))
  go (DbMaybe t)        = go t
  go (DbEmbedded _ ts)  = mapM_ (go . snd) ts
  go _                  = return ()    -- ordinary types need not migration
  f name mig cont = do
    v <- gets (Map.lookup name)
    case v of
      Nothing -> lift mig >>= modify.Map.insert name >> cont
      _ -> return ()
  allSubtypes = map snd . concatMap constrParams . constructors

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

-- Describes a database column. Field cType always contains DbType that maps to one column (no DbEmbedded)
data Column = Column
    { cName :: String
    , cNull :: Bool
    , cType :: DbType
    , cDefault :: Maybe String
    , cReference :: Maybe String -- table name
    } deriving (Eq, Show)

mkColumns :: String -> DbType -> [Column]
mkColumns columnName dbtype = go "" (columnName, dbtype) [] where
  go prefix (fname, typ) acc = case typ of
    DbEmbedded False ts -> foldr (go $ prefix ++ fname ++ "$") acc ts
    DbEmbedded True  ts -> foldr (go "") acc ts
    _          -> column:acc where
      column = Column (prefix ++ fname) isNullable simpleType Nothing ref
      (isNullable, simpleType, ref) = analyze typ
      analyze x = case x of
        DbMaybe a      -> let (_, t', ref') = analyze a in (True, t', ref')
        t@(DbEntity e) -> (False, t, Just $ entityName e)
        t@(DbList lName _)   -> (False, t, Just lName)
        DbEmbedded _ _ -> error "mkColumn: unexpected DbEmbedded"
        a              -> (False, a, Nothing)

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

data PSEmbeddedFieldDef = PSEmbeddedFieldDef {
    psEmbeddedFieldName :: String -- bar
  , psDbEmbeddedFieldName :: Maybe String -- SQLbar
  , psSubEmbedded :: Maybe [PSEmbeddedFieldDef]
} deriving Show

applyEmbeddedDbTypeSettings :: [PSEmbeddedFieldDef] -> DbType -> DbType
applyEmbeddedDbTypeSettings settings (DbEmbedded _ fields) = DbEmbedded True $ go settings fields where
  go [] [] = []
  go [] fs = fs
  go st [] = error $ "applyEmbeddedDbTypeSettings: embedded datatype does not have following fields: " ++ show st
  go st (f@(fName, fType):fs) = case find fName st of
    Just (rest, PSEmbeddedFieldDef _ dbName subs) -> (maybe fName id dbName, maybe id applyEmbeddedDbTypeSettings subs $ fType):go rest fs
    Nothing -> f:go st fs
  find :: String -> [PSEmbeddedFieldDef] -> Maybe ([PSEmbeddedFieldDef], PSEmbeddedFieldDef)
  find _ [] = Nothing
  find name (def:defs) | psEmbeddedFieldName def == name = Just (defs, def)
                       | otherwise = fmap (\(defs', result) -> (def:defs', result)) $ find name defs
applyEmbeddedDbTypeSettings _ t = error $ "applyEmbeddedDbTypeSettings: expected DbEmbedded, got " ++ show t
