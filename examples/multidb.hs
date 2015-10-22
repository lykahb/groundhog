{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell,
             QuasiQuotes, FlexibleInstances, FlexibleContexts #-}
               
-- Example of groundhog runnable against any of several
-- databases, showing handling of their connections.

import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.Groundhog.Sqlite
import Database.Groundhog.MySQL
import Database.Groundhog.Postgresql
import Database.Groundhog.TH
import System.Environment

data Customer = Customer {
      customerName :: String,
      phone :: String
    } deriving Show

-- The schema description is written in Template Haskell.
-- It describes which records are to be persisted,
-- and any other desired peculiarities of the database.
mkPersist defaultCodegenConfig [groundhog|
- entity: Customer               # Name of the datatype
|]

-- Convenience function for environment lookup.
getDefaultedEnv :: String -> String -> IO String
getDefaultedEnv envName defaultValue = do
  let lookupName = "GROUNDHOG_EXAMPLE_" ++ envName
  envValue <- lookupEnv lookupName
  case envValue of
    Nothing -> return defaultValue
    Just value -> return value

-- Default default username.
defaultUsername :: String
defaultUsername = "test"

-- Default default password.
defaultDatabase :: String
defaultDatabase = "groundhog_example"

getConnInfo :: IO (String, String, String)
getConnInfo = do
  username <- getDefaultedEnv "USERNAME" defaultUsername
  database <- getDefaultedEnv "DATABASE" defaultDatabase
  password <- getDefaultedEnv "PASSWORD" ""
  return (username, database, password)

-- Connection information for MySQL is kind of complicated.
makeMySQLConnInfo :: IO ConnectInfo
makeMySQLConnInfo = do
  (username, database, password) <- getConnInfo
  return $ ConnectInfo {
                      connectHost = "localhost",
                      connectPort = 3306,
                      connectUser = username,
                      connectPassword = password,
                      connectDatabase = database,
                      connectOptions = [],
                      connectPath = "",
                      connectSSL = Nothing
                    }

-- Connection information for Postgresql is string-formatted.
makePostgresqlConnInfo :: IO String
makePostgresqlConnInfo = do
  (username, database, password) <- getConnInfo
  return $
    "host=localhost port=5432 user=" ++ username ++
    " dbname=" ++ database ++ applyPassword password
  where
    applyPassword "" = ""
    applyPassword pw = " password=" ++ pw


-- The actual example simulates some purchases from an online store.
example :: (PersistBackend m, SqlDb (PhantomDb m), MonadIO m) => m ()
example = do

  -- Create database and table as needed.
  runMigration $ migrate (undefined :: Customer)

  -- Toy example, so throw away any old customer records
  -- to prevent them piling up in persistent databases.
  deleteAll (undefined :: Customer)

  -- Stick a customer into the database.
  johnKey <- insert $ Customer "John Doe" "0123456789"

  -- Get our customer back out and show him.
  johnRecord <- get johnKey
  liftIO $ print johnRecord

  -- Insert another customer.
  janeKey <- insert $ Customer "Jane Doe" "987654321"

  -- Find and list all customers matching a particular select:
  -- in this case, everybody.
  allCustomers <- select CondEmpty
  liftIO $ putStrLn $
    "All customers: " ++ show (allCustomers :: [Customer])


-- Sample driver.
main :: IO ()
main = do
  -- We will be database-independent, because we can.
  db <- getArgs
  case db of
    [] ->
        withSqliteConn ":memory:" $ runDbConn $ example
    ["sqlite"] ->
        withSqliteConn "example.sqlite3" $ runDbConn $ example
    ["mysql"] -> do
            mySQLConnInfo <- makeMySQLConnInfo
            withMySQLConn mySQLConnInfo $ runDbConn $ example
    ["postgresql"] -> do
            pgQLConnInfo <- makePostgresqlConnInfo
            withPostgresqlConn pgQLConnInfo $ runDbConn $ example
    _ -> error "unknown database"
