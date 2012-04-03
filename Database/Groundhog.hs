-- | This module exports the most commonly used functions and datatypes.
--
-- An example which shows the main features:
--
-- @
--{-\# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes \#-}
--import Control.Monad.IO.Class(liftIO)
--import Database.Groundhog.TH
--import Database.Groundhog.Sqlite
--
--data Customer a = Customer {customerName :: String, details :: a} deriving Show
--data Item = Product {productName :: String, quantity :: Int, customer :: Customer String}
--          | Service {serviceName :: String, deliveryAddress :: String, servicePrice :: Int}
--     deriving Show
--
--'mkPersist' fieldNamingStyle [groundhog|
-- - entity: Customer
--   constructors:
--     - name: Customer
--       constraints:
--         - name: NameConstraint
--           fields: [customerName]
-- - entity: Item
-- |]
--
--main = withSqliteConn \":memory:\" $ runSqliteConn $ do
--  -- Customer is also migrated because Item contains it
--  'runMigration' 'silentMigrationLogger' $ 'migrate' (undefined :: Item)
--  let john = Customer \"John Doe\" \"Phone: 01234567\"
--  johnKey <- 'insert' john
--  -- John is inserted only once because of the name constraint
--  insert $ Product \"Apples\" 5 john
--  insert $ Product \"Melon\" 2 john
--  insert $ Service \"Taxi\" \"Elm Street\" 50
--  insert $ Product \"Melon\" 6 (Customer \"Jack Smith\" \"Don't let him pay by check\")
--  -- bonus melon for all large melon orders
--  'update' [QuantityField '=.' toArith QuantityField + 1] (ProductNameField '==.' \"Melon\" '&&.' QuantityField '>.' (5 :: Int))
--  productsForJohn <- 'select' (CustomerField ==. johnKey) [] 0 0
--  liftIO $ putStrLn $ \"Products for John: \" ++ show productsForJohn
--  -- check bonus
--  melon <- select (ProductNameField ==. \"Melon\") ['Desc' QuantityField] 0 0
--  liftIO $ putStrLn $ \"Melon orders: \" ++ show melon
-- @
module Database.Groundhog
  ( module Database.Groundhog.Core
  , module Database.Groundhog.Generic
  ) where

import Database.Groundhog.Core
  ( PersistBackend(..)
  , DbPersist(..)
  , Key(..)
  , Cond(..)
  , Order(..)
  , (=.), (&&.), (||.), (==.), (/=.), (<.), (<=.), (>.), (>=.)
  , wrapPrim
  , toArith)
import Database.Groundhog.Generic
  ( createMigration
  , executeMigration
  , executeMigrationUnsafe
  , runMigration
  , runMigrationUnsafe
  , printMigration
  , silentMigrationLogger
  , defaultMigrationLogger)

import Database.Groundhog.Instances ()