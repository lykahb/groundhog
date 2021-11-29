{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.Sqlite
import Database.Groundhog.TH

data Customer = Customer {customerName :: String, phone :: String} deriving (Show)

data Product = Product {productName :: String, quantity :: Int, customer :: DefaultKey Customer}

mkPersist
  defaultCodegenConfig
  [groundhog|
- entity: Customer               # Name of the datatype
  constructors:
    - name: Customer
      fields:
        - name: customerName
          dbName: name           # Set column name to "name" instead of "customerName"
- entity: Product
|]

deriving instance Show Product

main = withSqliteConn ":memory:" $
  runDbConn $ do
    -- create tables
    runMigration $ do
      migrate (undefined :: Customer)
      migrate (undefined :: Product)
    johnKey <- insert $ Customer "John Doe" "0123456789"
    janeKey <- insert $ Customer "Jane Doe" "987654321"
    -- get customer by its primary key
    get johnKey >>= liftIO . print
    -- insert objects with a foreign key
    insert $ Product "Apples" 5 johnKey
    insert $ Product "Melon" 3 johnKey
    insert $ Product "Oranges" 4 janeKey
    -- select all customers without condition
    allCustomers <- select CondEmpty
    liftIO $ putStrLn $ "All customers: " ++ show (allCustomers :: [Customer])
    -- bonus melon for all large melon orders. The values used in expressions should have defined type, so literal 5 is annotated.
    update [QuantityField =. liftExpr QuantityField + 1] (ProductNameField ==. "Melon" &&. QuantityField >. (5 :: Int))
    productsForJohn <- select $ (CustomerField ==. johnKey) `orderBy` [Asc ProductNameField]
    liftIO $ putStrLn $ "Products for John: " ++ show productsForJohn
