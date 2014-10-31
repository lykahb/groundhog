{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data Customer = Customer {customerName :: String, phone :: String} deriving Show
data Product = Product {productName :: String, quantity :: Int, customer :: DefaultKey Customer}
deriving instance Show Product

mkPersist defaultCodegenConfig [groundhog|
- entity: Customer               # Name of the datatype
  constructors:
    - name: Customer
      fields:
        - name: customerName
          dbName: name           # Set column name to "name" instead of "customerName"
- entity: Product
|]

main = withSqliteConn ":memory:" $ runDbConn $ do
  runMigration $ do
    migrate (undefined :: Customer)
    migrate (undefined :: Product)
  johnKey <- insert $ Customer "John Doe" "0123456789"
  get johnKey >>= liftIO . print
  insert $ Product "Apples" 5 johnKey
  insert $ Product "Melon" 3 johnKey
  janeKey <- insert $ Customer "Jane Doe" "987654321"
  allCustomers <- select CondEmpty
  liftIO $ putStrLn $ "All customers: " ++ show (allCustomers :: [Customer])
  insert $ Product "Oranges" 4 janeKey
  -- bonus melon for all large melon orders. The values used in expressions should have known type, so literal 5 is annotated.
  update [QuantityField =. liftExpr QuantityField + 1] (ProductNameField ==. "Melon" &&. QuantityField >. (5 :: Int))
  productsForJohn <- select $ (CustomerField ==. johnKey) `orderBy` [Asc ProductNameField]
  liftIO $ putStrLn $ "Products for John: " ++ show productsForJohn
