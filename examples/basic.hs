{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell,
             QuasiQuotes, FlexibleInstances, FlexibleContexts,
             StandaloneDeriving #-}
               
-- Toy example of groundhog use.

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

data Product = Product {
      productName :: String,
      quantity :: Int,
      customer :: DefaultKey Customer
    }

-- The standalone deriving is to get customer handled correctly.
deriving instance Show Product

-- The schema description is written in Template Haskell.
-- It describes which records are to be persisted,
-- and any other desired peculiarities of the database.
mkPersist defaultCodegenConfig [groundhog|
- entity: Customer               # Name of the datatype
  constructors:
    - name: Customer
      fields:
        - name: customerName
          # Set column name to "name" instead of "customerName"
          dbName: name
- entity: Product
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
  -- Create database and tables as needed.
  runMigration $ do
    migrate (undefined :: Customer)
    migrate (undefined :: Product)

  -- Toy example, so drop any previous state.
  deleteAll (undefined :: Product)
  deleteAll (undefined :: Customer)

  -- Stick a customer into the database.
  johnKey <- insert $ Customer "John Doe" "0123456789"

  -- Get our customer back out and show him.
  johnRecord <- get johnKey
  liftIO $ print johnRecord

  -- Have our customer buy some stuff.
  _ <- insert $ Product "Apples" 5 johnKey
  _ <- insert $ Product "Melon" 3 johnKey

  -- Insert another customer.
  janeKey <- insert $ Customer "Jane Doe" "987654321"

  -- Find and list all customers matching a particular select:
  -- in this case, everybody.
  allCustomers <- select CondEmpty
  liftIO $ putStrLn $
             "All customers: " ++ show (allCustomers :: [Customer])

  -- Have our new customer buy some stuff.
  _ <- insert $ Product "Oranges" 4 janeKey

  -- Bonus melon for all large melon orders. The values
  -- used in expressions should have known type, so
  -- literal 5 is annotated.
  update [QuantityField =. liftExpr QuantityField + 1]
             (ProductNameField ==. "Melon" &&. QuantityField >. (5 :: Int))

  -- Get and show first customer's order sorted by product name.
  productsForJohn <-
      select $ (CustomerField ==. johnKey) `orderBy` [Asc ProductNameField]
  liftIO $ putStrLn $ "Products for John: " ++ show productsForJohn

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
