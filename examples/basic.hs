{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell,
             QuasiQuotes, FlexibleInstances #-}

-- Toy example of groundhog use.

import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.Sqlite
import Database.Groundhog.TH

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

-- The example manipulates some customer records.
main :: IO ()
main = withSqliteConn ":memory:" $ runDbConn $ do

  -- Create database and table as needed.
  runMigration $ migrate (undefined :: Customer)

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
