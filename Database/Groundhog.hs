-- | This module exports the most commonly used functions and datatypes.
--
-- An example which shows the main features:
--
-- @
-- {-\# LANGUAGE GADTs, TypeFamilies, TemplateHaskell \#-}
-- import Control.Monad.IO.Class(liftIO)
-- import Database.Groundhog.Sqlite
-- import Database.Groundhog.TH
-- 
-- data Customer a = Customer {customerName :: String, details :: a} deriving Show
-- data Item = ProductItem {productName :: String, quantity :: Int, customer :: Customer String}
--           | ServiceItem {serviceName :: String, deliveryAddress :: String, servicePrice :: Int}
--      deriving Show
-- 
-- 'deriveEntity' ''Customer $ Just $ do
--   setConstructor 'Customer $ do
--     setConstraints [(\"NameConstraint\", [\"customerName\"])]
-- deriveEntity ''Item Nothing
-- 
-- main = withSqliteConn \":memory:\" $ runSqliteConn $ do
--   -- Customer is also migrated because Item contains it
--   'runMigration' 'silentMigrationLogger' $ 'migrate' (undefined :: Item)
--   let john = Customer \"John Doe\" \"Phone: 01234567\"
--   johnKey <- 'insert' john
--   -- John is inserted only once because of the name constraint
--   insert $ ProductItem \"Apples\" 5 john
--   insert $ ProductItem \"Melon\" 2 john
--   insert $ ServiceItem \"Taxi\" \"Elm Street\" 50
--   insert $ ProductItem \"Melon\" 6 (Customer \"Jack Smith\" \"Don't let him pay by check\")
--   -- bonus melon for all large melon orders
--   'update' [QuantityField '=.' toArith QuantityField + 1] (ProductNameField '==.' \"Melon\" '&&.' QuantityField '>.' (5 :: Int))
--   productsForJohn <- 'select' (CustomerField ==. johnKey) [] 0 0
--   liftIO $ putStrLn $ \"Products for John: \" ++ show productsForJohn
--   -- let's check bonus
--   melon <- select (ProductNameField ==. \"Melon\") ['Desc' QuantityField] 0 0
--   liftIO $ putStrLn $ \"Melon orders: \" ++ show melon
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

import Database.Groundhog.TH