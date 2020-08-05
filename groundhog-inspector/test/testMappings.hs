{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Database.Groundhog.Core
import Database.Groundhog.MySQL
import Database.Groundhog.Postgresql
import Database.Groundhog.Sqlite
import Database.Groundhog.TH
import Datatypes
import System.Environment (getArgs)

mkPersist
  (defaultCodegenConfig {migrationFunction = Just "mig"})
  [groundhogFile|mapping.yml|]

main :: IO ()
main = do
  stmts' <- liftIO $ getArgs >>= readFile . head
  withSqliteConn ":memory:" $
    runDbConn $ do
      -- withPostgresqlConn "dbname=test user=test password=test host=localhost" $ runDbConn $ do
      -- let mySQLConnInfo = defaultConnectInfo
      --                   { connectHost     = "localhost"
      --                   , connectUser     = "test"
      --                   , connectPassword = "test"
      --                   , connectDatabase = "test"
      --                   }
      -- withMySQLConn mySQLConnInfo $ runDbConn $ cleanMySQL >> do
      let stmts = filter ((\s -> not $ null s || "--" `isPrefixOf` s) . dropWhile isSpace) . lines $ stmts'
      mapM_ (\s -> executeRaw False s []) stmts
      createMigration mig >>= printMigration
      executeRaw False "ROLLBACK" [] >> executeRaw False "BEGIN" []

cleanMySQL :: PersistBackend m => m ()
cleanMySQL = do
  executeRaw True "SET FOREIGN_KEY_CHECKS = 0" []
  let recreate schema = do
        executeRaw True ("drop database if exists " ++ schema) []
        executeRaw True ("create database " ++ schema) []
  mapM_ recreate ["test", "myschema"]
  executeRaw True "use test" []
  executeRaw True "SET FOREIGN_KEY_CHECKS = 1" []
