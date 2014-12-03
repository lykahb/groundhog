{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map

import Database.Groundhog.Generic.Migration (SchemaAnalyzer)
import Database.Groundhog.TH (suffixNamingStyle)
import Database.Groundhog.Inspector

#if WITH_SQLITE
import Database.Groundhog.Sqlite
#endif
#if WITH_POSTGRESQL
import Database.Groundhog.Postgresql
#endif
#if WITH_MYSQL
import Database.Groundhog.MySQL
#endif

data Args = Args {database :: String, connectionInfo :: String} deriving (Show, Data, Typeable)

databases :: [String]
databases =
#if WITH_SQLITE
  "sqlite":
#endif
#if WITH_POSTGRESQL
  "postgresql":
#endif
#if WITH_MYSQL
  "mysql":
#endif
  []

sample :: Mode (CmdArgs Args)
sample = cmdArgsMode $ Args { database = def &= argPos 0 &= typ (show databases) &= opt (head databases)
              , connectionInfo = def &= argPos 1 &= typ "CONNECTION_STRING" }
         &= summary "groundhog-inspector"
         &= details ["Pass a name of a database. The connection string is an argument to with*Conn. "
           , "MySQL connection string is \"ConnectInfo {...}\""]

analyze :: (PersistBackend m, SchemaAnalyzer m, MonadIO m) => m ()
analyze = do
  tables <- collectTables (const True) Nothing
  -- Analyze tables
  let decs = generateData defaultDataCodegenConfig defaultReverseNamingStyle tables
  mappings <- generateMapping defaultReverseNamingStyle tables
  -- Print datatype declarations
  liftIO $ mapM_ (putStrLn . showData) $ concat $ map (uncurry (:)) $ Map.elems $ decs
  -- Remove parts of mapping that are defaults for the chosen naming style
  let mappings' = Map.intersectionWith (minimizeMapping suffixNamingStyle . fst) decs mappings
  -- Print mappings
  liftIO $ B.putStrLn $ showMappings $ Map.elems mappings'

main :: IO ()
main = do
  arg <- cmdArgsRun sample
  case database arg of
#if WITH_SQLITE
    "sqlite" -> withSqliteConn (connectionInfo arg) $ runDbConn analyze
#endif
#if WITH_POSTGRESQL
    "postgresql" -> withPostgresqlConn (connectionInfo arg) $ runDbConn analyze
#endif
#if WITH_MYSQL
    "mysql" -> withMySQLConn (read $ connectionInfo arg) $ runDbConn analyze
#endif
    other -> fail $ "Unknown database: " ++ other