import Control.Arrow (first)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import Database.Groundhog.Generic.Migration (getMigrationPack)
import Database.Groundhog.Inspector
import Database.Groundhog.MySQL
import Database.Groundhog.Postgresql
import Database.Groundhog.Sqlite
import Database.Groundhog.TH
import Language.Haskell.TH
import System.Environment (getArgs)

main :: IO ()
main = do
  stmts' <- getArgs >>= readFile . head
  (decs, mappings) <- withSqliteConn ":memory:" $
    runDbConn $ do
      -- (decs, mappings) <- withPostgresqlConn "dbname=test user=test password=test host=localhost" $ runDbConn $ do
      -- let mySQLConnInfo = defaultConnectInfo
      --                   { connectHost     = "localhost"
      --                   , connectUser     = "test"
      --                   , connectPassword = "test"
      --                   , connectDatabase = "test"
      --                   }
      -- (decs, mappings) <- withMySQLConn mySQLConnInfo $ runDbConn $ cleanMySQL >> do
      let stmts = filter ((\s -> not $ null s || "--" `isPrefixOf` s) . dropWhile isSpace) . lines $ stmts'
      mapM_ (\s -> executeRaw False s []) stmts
      tables <- collectTables (\(_, name) -> not $ "_not_mapped" `isSuffixOf` name) Nothing
      executeRaw False "ROLLBACK" [] >> executeRaw False "BEGIN" []
      let decs = generateData defaultDataCodegenConfig defaultReverseNamingStyle tables
      mappings <- generateMapping defaultReverseNamingStyle tables
      pure (decs, mappings)
  --  mapM_ (putStrLn . showData) decs
  writeFile "Datatypes.hs" $
    "{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}\n"
      ++ "module Datatypes where\n"
      ++ "import Database.Groundhog\n"
      ++ "import Data.ByteString.Lazy.Char8 (ByteString)\n"
      ++ "import Data.Int (Int32, Int64)\n"
      ++ "import Data.Time (Day, TimeOfDay, UTCTime)\n"
      ++ "import Data.Time.LocalTime (ZonedTime)\n"
      ++ unlines (map showData $ concatMap (uncurry (:)) $ Map.elems decs)
  let mappings' = Map.intersectionWith (minimizeMapping suffixNamingStyle . fst) decs mappings
  let serializedMappings = showMappings $ Map.elems mappings'
  --  B.putStrLn serializedMappings
  B.writeFile "mapping.yml" serializedMappings

cleanMySQL :: PersistBackend m => m ()
cleanMySQL = do
  executeRaw True "SET FOREIGN_KEY_CHECKS = 0" []
  let recreate schema = do
        executeRaw True ("drop database if exists " ++ schema) []
        executeRaw True ("create database " ++ schema) []
  mapM_ recreate ["test", "myschema"]
  executeRaw True "use test" []
  executeRaw True "SET FOREIGN_KEY_CHECKS = 1" []
