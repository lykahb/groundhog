{-# LANGUAGE FlexibleContexts #-}

module Groundhog.Test.Specs.SqliteSpec where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (isInfixOf, sort)
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic.Migration (SchemaAnalyzer (..))
import Database.Groundhog.Sqlite
import Groundhog.Test.Types.Types
import Groundhog.Test.Util
import Test.Hspec (SpecWith, expectationFailure, it)

spec :: (ExtractConnection cm Sqlite) => SpecWith cm
spec = do
  it "migrates and analyzes a trigger" $
    runDbConnIO $ do
      let val = Single (Single "abc")
      migr val
      let action = "select * from \"Single#String\";"
      executeRaw False ("CREATE TRIGGER \"myTrigger\" AFTER DELETE ON \"Single#String\" FOR EACH ROW BEGIN " ++ action ++ " END") []
      ["Single#Single#String", "Single#String"] @=?? fmap sort (listTables Nothing)
      ["myTrigger"] @=?? fmap sort (listTableTriggers (Nothing, "Single#String"))
      sql <- analyzeTrigger (Nothing, "myTrigger")
      let sql' = maybe (error "No trigger found") id sql
      unless (action `isInfixOf` sql') $
        liftIO $ expectationFailure "Trigger does not contain action statement"
