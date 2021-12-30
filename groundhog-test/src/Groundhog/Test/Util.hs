{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Groundhog.Test.Util where

import qualified Control.Exception as E
import Control.Exception.Base (SomeException)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Data.Map as Map
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic (phantomDb)
import Test.Hspec (expectationFailure, shouldBe)

migr :: (PersistBackend m, PersistEntity v) => v -> m ()
migr v = do
  m1 <- createMigration $ migrate v
  executeMigration m1
  m2 <- createMigration $ migrate v
  let secondMigration = filter (/= Right []) (Map.elems m2)
  unless (null secondMigration) $
    liftIO $ expectationFailure $ "first migration: " ++ show (Map.elems m1) ++ "\nsecond migration:" ++ show secondMigration

assertExc :: (PersistBackend m, MonadBaseControl IO m) => String -> m a -> m ()
assertExc err m = do
  happened <- control $ \runInIO -> E.catch (runInIO $ m >> pure False) (\(_ :: SomeException) -> runInIO $ pure True)
  unless happened $ liftIO (expectationFailure err)

runDbConnIO :: (ConnectionManager conn, ExtractConnection cm conn) => Action conn a -> cm -> IO a
runDbConnIO = runDbConn

-- TODO: replace this operator with a function
(@=??) :: (Eq a, Show a, MonadIO m) => a -> m a -> m ()
expected @=?? action = action >>= \actual -> liftIO $ actual `shouldBe` expected

reescape :: DbDescriptor db => proxy db -> String -> String
reescape proxy query =
  if backendName proxy == "mysql"
    then map (\c -> if c == '"' then '`' else c) query
    else query

executeRaw' :: PersistBackend m => String -> [PersistValue] -> m ()
executeRaw' query vals = do
  proxy <- phantomDb
  executeRaw True (reescape proxy query) vals

queryRaw' :: PersistBackend m => String -> [PersistValue] -> m (RowStream [PersistValue])
queryRaw' query vals = do
  proxy <- phantomDb
  queryRaw True (reescape proxy query) vals
