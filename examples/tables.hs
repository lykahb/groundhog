{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell,
             QuasiQuotes, FlexibleInstances,
             StandaloneDeriving #-}
               
-- Example of groundhog use with multiple connected tables.

import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.Sqlite
import Database.Groundhog.TH

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

-- The example simulates some purchases from an online store.
main :: IO ()
main = withSqliteConn ":memory:" $ runDbConn $ do

  -- Create database and tables as needed.
  runMigration $ do
    migrate (undefined :: Customer)
    migrate (undefined :: Product)

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
