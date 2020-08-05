{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Database.Groundhog.Core
import Database.Groundhog.Sqlite
import Database.Groundhog.TH

data Customer = Customer {customerName :: String, customerPhone :: String} deriving (Show)

data TestException = TestException
  deriving (Show)

instance Exception TestException

mkPersist
  defaultCodegenConfig
  [groundhog|
- entity: Customer               # Name of the datatype
  constructors:
    - name: Customer
      uniques:
        - name: PhoneConstraint
          fields: [ customerPhone ]
|]

migration :: PersistBackendConn conn => Action conn ()
migration = runMigration $ do
  migrate (undefined :: Customer)

insertCustomer :: (MonadIO m, PersistBackendConn conn) => Customer -> TryAction TestException m conn (Key Customer BackendSpecific)
insertCustomer c = insert c

insertFails :: (MonadIO m, PersistBackendConn conn) => TryAction TestException m conn (Key Customer BackendSpecific)
insertFails = do
  janeKey <- insert $ Customer "Jane Doe" "987654321"
  -- Some logic leads to exception
  lift $ throwE TestException
  return janeKey

main = withSqliteConn ":memory:" $ \conn -> do
  -- create tables
  runDbConn migration conn >>= liftIO . print

  let customer = Customer "John Doe" "0123456789"

  -- Use runTryDbConn to run a TryAction.
  -- Insert that is successful
  runTryDbConn (insertCustomer customer) conn >>= liftIO . print

  -- Insert that fails because of the unique condition
  runTryDbConn (insertCustomer customer) conn >>= liftIO . print

  -- Insert that fails because of the exception
  runTryDbConn insertFails conn >>= liftIO . print

  -- Use runTryDbConn' to run an Action and catch the exception if it fails
  runTryDbConn' (insert customer) conn >>= liftIO . print

  -- Select all customers confirms that the failing TryActions and Action were rolled back
  runDbConn (select CondEmpty) conn >>= liftIO . (print :: [Customer] -> IO ())
