{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Groundhog.Test.Specs.MySQLSpec where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (isInfixOf, sort)
import Data.Maybe (fromMaybe)
import Database.Groundhog.Core
import Database.Groundhog.Generic.Migration (SchemaAnalyzer (..))
import Database.Groundhog.MySQL
import Groundhog.Test.Types.Types
import Groundhog.Test.Util
import Test.Hspec (SpecWith, expectationFailure, it, shouldBe)

spec :: (ExtractConnection cm MySQL) => SpecWith cm
spec = do
  it "migrates and analyzes functions and triggers" $
    runDbConnIO $ do
      let val = Single (Single "abc")
      migr val
      let action = "delete from `Single#String`;"
      executeRaw' ("CREATE TRIGGER `myTrigger` AFTER DELETE ON `Single#String` FOR EACH ROW BEGIN " ++ action ++ " END") []
      ["Single#Single#String", "Single#String"] @=?? fmap sort (listTables Nothing)
      ["myTrigger"] @=?? fmap sort (listTableTriggers (Nothing, "Single#String"))
      sql <- analyzeTrigger (Nothing, "myTrigger")
      let sql' = maybe (error "No trigger found") id sql
      unless (action `isInfixOf` sql') $
        liftIO $ expectationFailure "Trigger does not contain action statement"
      executeRaw' "CREATE FUNCTION myfunc() RETURNS decimal DETERMINISTIC BEGIN RETURN 42;END" []
      func <- analyzeFunction (Nothing, "myfunc")
      let (_, ret, body) = fromMaybe (error "No function found") func
      liftIO $ ret `shouldBe` Just (DbOther (OtherTypeDef [Left "decimal(10,0)"]))
      unless ("RETURN 42" `isInfixOf` body) $
        liftIO $ expectationFailure "Function does not contain action statement"

-- DDL statements are committed automatically so we cannot rollback them.
cleanMySQL :: (PersistBackend m, Conn m ~ MySQL) => m ()
cleanMySQL = do
  executeRaw True "SET FOREIGN_KEY_CHECKS = 0" []
  forM_ ["test", "myschema"] $ \schema -> do
    executeRaw True ("drop database if exists " ++ schema) []
    executeRaw True ("create database " ++ schema) []
  executeRaw True ("use test") []
  executeRaw True "SET FOREIGN_KEY_CHECKS = 1" []
