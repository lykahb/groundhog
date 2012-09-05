{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances #-}
import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data Customer a = Customer {customerName :: String, remark :: a} deriving Show
data Product = Product {productName :: String, quantity :: Int, customer :: Customer String} deriving Show

mkPersist suffixNamingStyle [groundhog|
- entity: Customer
  constructors:
    - name: Customer
      constraints:
        - name: NameConstraint
          fields: [customerName]
- entity: Product
|]

main = withSqliteConn ":memory:" $ runSqliteConn $ do
  -- Customer is also migrated because Product references it
  runMigration defaultMigrationLogger $ migrate (undefined :: Product)
  let john = Customer "John Doe" "Phone: 01234567"
  johnKey <- insert john
  -- John is inserted only once because of the name constraint
  insert $ Product "Apples" 5 john
  insert $ Product "Melon" 2 john
  insert $ Product "Melon" 6 (Customer "Jack Smith" "Don't let him pay by check")
  -- bonus melon for all large melon orders. The values used in expressions should have known type, so literal 5 is annotated.
  update [QuantityField =. toArith QuantityField + 1] (ProductNameField ==. "Melon" &&. QuantityField >. (5 :: Int))
  productsForJohn <- select (CustomerField ==. johnKey) [] 0 0
  liftIO $ putStrLn $ "Products for John: " ++ show productsForJohn
  -- check bonus
  melon <- select (ProductNameField ==. "Melon") [Desc QuantityField] 0 0
  liftIO $ putStrLn $ "Melon orders: " ++ show melon
