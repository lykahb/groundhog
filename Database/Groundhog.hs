-- | This module exports the most commonly used functions and datatypes.
--
-- The example below shows the most of the main features. See more examples in the examples directory.
--
-- @
--{-\# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances \#-}
--import Control.Monad.IO.Class (liftIO)
--import Database.Groundhog.TH
--import Database.Groundhog.Sqlite
--
--data Customer a = Customer {customerName :: String, remark :: a} deriving Show
--data Product = Product {productName :: String, quantity :: Int, customer :: Customer String} deriving Show
--
--'mkPersist' ('defaultCodegenConfig' {migrationFunction = Just \"migrateAll\"}) [groundhog|
-- - entity: Customer
--   constructors:
--     - name: Customer
--       uniques:
--         - name: NameConstraint
--           fields: [customerName]
-- - entity: Product
-- |]
--
--main = withSqliteConn \":memory:\" $ runDbConn $ do
--  -- Customer is also migrated because Product references it.
--  -- It is possible to migrate schema for given type, e.g. migrate (undefined :: Customer String), or run migrateAll
--  'runMigration' 'defaultMigrationLogger' migrateAll
--  let john = Customer \"John Doe\" \"Phone: 01234567\"
--  johnKey <- 'insert' john
--  -- John is inserted only once because of the name constraint
--  insert $ Product \"Apples\" 5 john
--  insert $ Product \"Melon\" 2 john
--  -- Groundhog prevents SQL injections. Quotes and other special symbols are safe.
--  insert $ Product \"Melon\" 6 (Customer \"Jack Smith\" \"Don't let him pay by check\")
--  -- Bonus melon for all large melon orders. The values used in expressions should have known type, so literal 5 is annotated.
--  'update' [QuantityField '=.' toArith QuantityField + 1] (ProductNameField '==.' \"Melon\" '&&.' QuantityField '>.' (5 :: Int))
--  productsForJohn <- 'select' $ CustomerField ==. johnKey
--  liftIO $ putStrLn $ \"Products for John: \" ++ show productsForJohn
--  -- Check bonus
--  melon <- select $ (ProductNameField ==. \"Melon\") \`orderBy\` ['Desc' QuantityField]
--  liftIO $ putStrLn $ \"Melon orders: \" ++ show melon
-- @
module Database.Groundhog (module G) where

import Database.Groundhog.Core as G
  ( PersistBackend(..)
  , DbPersist(..)
  , Key(..)
  , Unique
  , BackendSpecific
  , extractUnique
  , Cond(..)
  , Order(..)
  , Selector(..)
  , AutoKeyField(..)
  , (~>)
  , limitTo
  , offsetBy
  , orderBy)
import Database.Groundhog.Expression as G
import Database.Groundhog.Generic as G
  ( createMigration
  , executeMigration
  , executeMigrationUnsafe
  , runMigration
  , runMigrationUnsafe
  , printMigration
  , silentMigrationLogger
  , defaultMigrationLogger)

import Database.Groundhog.Instances as G
