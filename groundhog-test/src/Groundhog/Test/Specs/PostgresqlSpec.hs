{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Groundhog.Test.Specs.PostgresqlSpec where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as A
import Data.List (isInfixOf, sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as TextStrict
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic.Migration (SchemaAnalyzer (..))
import Database.Groundhog.Postgresql
import Database.Groundhog.Postgresql.Array hiding (all, any, append)
import qualified Database.Groundhog.Postgresql.Array as Arr
import Database.Groundhog.Postgresql.Geometry hiding ((&&), (>>))
import qualified Database.Groundhog.Postgresql.HStore as HStore
import Groundhog.Test.Types.Types
import Groundhog.Test.Util
import Test.Hspec (SpecWith, expectationFailure, it, shouldBe)

spec :: (ExtractConnection cm Postgresql) => SpecWith cm
spec = do
  it "works with geometric data types" $
    runDbConnIO $ do
      let p = Point 1.2 3.45
      let val1 = Single (p, Lseg p p, Box p p, Circle p 6.7)
      let val2 = Single (OpenPath [p], ClosedPath [p], OpenPath [p, p], ClosedPath [p, p], (Polygon [p], Polygon [p, p]))
      migr val1
      k1 <- insert val1
      val1' <- get k1
      liftIO $ val1' `shouldBe` Just val1
      migr val2
      k2 <- insert val2
      val2' <- get k2
      liftIO $ val2' `shouldBe` Just val2

  it "works with arrays" $
    runDbConnIO $ do
      let val = Single (Array [1 :: Int, 2, 3])
      migr val
      k <- insert val
      val' <- get k
      liftIO $ val' `shouldBe` Just val
      -- test operators
      let int :: Int -> Int
          int = id
      [2] @=?? project (SingleField ! int 2) (AutoKeyField ==. k)
      [Array [2, 3]] @=?? project (SingleField !: (int 2, int 3)) (AutoKeyField ==. k)
      [Array [1 .. 4]] @=?? project (SingleField `Arr.append` explicitType (int 4)) (AutoKeyField ==. k)
      [Array [0 .. 3]] @=?? project (explicitType (int 0) `Arr.prepend` SingleField) (AutoKeyField ==. k)
      [Array [1 .. 6]] @=?? project (SingleField `arrayCat` Array [int 4 .. 6]) (AutoKeyField ==. k)
      ["[1:3]"] @=?? project (arrayDims SingleField) (AutoKeyField ==. k)
      [1] @=?? project (arrayNDims SingleField) (AutoKeyField ==. k)
      [1] @=?? project (arrayLower SingleField 1) (AutoKeyField ==. k)
      [3] @=?? project (arrayUpper SingleField 1) (AutoKeyField ==. k)
      [3] @=?? project (arrayLength SingleField 1) (AutoKeyField ==. k)
      ["1|2|3"] @=?? project (arrayToString SingleField "|") (AutoKeyField ==. k)
      [k] @=?? project AutoKeyField (AutoKeyField ==. k &&. Arr.any (int 2) SingleField)
      [] @=?? project AutoKeyField (AutoKeyField ==. k &&. Arr.any (int 0) SingleField)
      [k] @=?? project AutoKeyField (AutoKeyField ==. k &&. Arr.all (int 2) (SingleField !: (int 2, int 2)))
      [] @=?? project AutoKeyField (AutoKeyField ==. k &&. Arr.all (int 2) SingleField)
      [k] @=?? project AutoKeyField (AutoKeyField ==. k &&. SingleField Arr.@> Array [int 2, 1])
      [k] @=?? project AutoKeyField (AutoKeyField ==. k &&. Array [int 2, 1] Arr.<@ SingleField)
      [k] @=?? project AutoKeyField (AutoKeyField ==. k &&. Array [int 3 .. 10] `overlaps` SingleField)
      -- test escaping/unescaping
      let myString = ['\1' .. '\255'] :: String
      let val2 = Single (Array [myString], Array [Array [myString]])
      migr val2
      Just val2 @=?? (insert val2 >>= get)
      Just (Array [2, 3, 20]) @=?? pure (A.decode (A.encode (Array [(2 :: Int), 3, 20])) :: Maybe (Array Int))

  it "migrates and analyzes functions and triggers" $
    runDbConnIO $ do
      let val = Single (Single "abc")
      migr val
      let action = "EXECUTE PROCEDURE \"myFunction\"()"
      executeRaw False "CREATE OR REPLACE FUNCTION \"myFunction\"() RETURNS trigger AS $$ BEGIN RETURN NEW;END; $$ LANGUAGE plpgsql" []
      executeRaw False ("CREATE TRIGGER \"myTrigger\" AFTER DELETE ON \"Single#String\" FOR EACH ROW " ++ action) []
      ["Single#Single#String", "Single#String"] @=?? fmap sort (listTables Nothing)
      ["myTrigger"] @=?? fmap sort (listTableTriggers (Nothing, "Single#String"))
      trigSql <- analyzeTrigger (Nothing, "myTrigger")
      let trigSql' = maybe (error "No trigger found") id trigSql
      -- use function name instead of action because Postgres 11 returns different action with "EXECUTE FUNCTION"
      unless ("\"myFunction\"()" `isInfixOf` trigSql') $
        liftIO $ expectationFailure "Trigger does not contain action statement"
      func <- analyzeFunction (Nothing, "myFunction")
      let (args, ret, body) = fromMaybe (error "No function found") func
      liftIO $ args `shouldBe` Just []
      liftIO $ ret `shouldBe` Just (DbOther (OtherTypeDef [Left "trigger"]))
      unless ("RETURN NEW;" `isInfixOf` body) $
        liftIO $ expectationFailure "Function does not contain action statement"

  it "works with DISTINCT ON" $
    runDbConnIO $ do
      let val1 = Single (5 :: Int, "abc")
          val2 = Single (6 :: Int, "123")
      migr val1
      insert_ val1
      insert_ $ Single (5 :: Int, "def")
      insert_ val2
      [val1, val2] @=?? (select $ CondEmpty `distinctOn` (SingleField ~> Tuple2_0Selector) `orderBy` [Asc $ SingleField ~> Tuple2_0Selector, Asc $ SingleField ~> Tuple2_1Selector])

  it "works with HStore" $
    runDbConnIO $ do
      let val = Single (HStore.HStore $ Map.fromList [(TextStrict.pack "k", TextStrict.pack "v")])
      migr val
      k <- insert val
      show (Just val) @=?? (fmap show $ get k) -- HStore does not have Eq, compare by show
      [True] @=?? project (HStore.exist SingleField "k") (AutoKeyField ==. k)

  it "migrates index that has expressions" $
    runDbConnIO $ do
      let val = ExpressionIndex 1
      migr val
      Just val @=?? (insert val >>= get)
      assertExc "expression index should fail on duplicates" $ insert $ ExpressionIndex (-1)

cleanPostgresql :: (PersistBackend m, Conn m ~ Postgresql) => m ()
cleanPostgresql = do
  executeRaw True "rollback" []
  executeRaw True "begin" []
