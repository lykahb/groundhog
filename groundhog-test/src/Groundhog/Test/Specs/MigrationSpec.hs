module Groundhog.Test.Specs.MigrationSpec where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function (on)
import Data.List (intercalate, isInfixOf)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic (firstRow, haveSameElems, phantomDb, streamToList)
import Database.Groundhog.Generic.Migration (Column (..), Reference (..), SchemaAnalyzer (..), TableInfo (..))
import qualified Groundhog.Test.Types.NewMigration as New
import qualified Groundhog.Test.Types.OldMigration as Old
import Groundhog.Test.Types.Types
import Groundhog.Test.Util
import Test.Hspec (SpecWith, expectationFailure, it, shouldBe)

spec :: (PersistBackendConn conn, ExtractConnection cm conn, SchemaAnalyzer conn) => SpecWith cm
spec = do
  it "raises exception when some constructor tables exist" $
    runDbConnIO $ do
      migr (undefined :: Multi String)
      executeRaw False "drop table \"Multi#String\"" []
      assertExc "Migration must fail in presence of orphan constructor tables" $ migr (undefined :: Multi String)

  it "applies schema settings" $
    runDbConnIO $ do
      proxy <- phantomDb
      let val = Single "def"
      migr val
      singleKey <- insert val
      let settable = Settable "abc" (Just singleKey) (1, ("qqq", Nothing))
      m <- fmap (Map.lookup "entity sqlsettable") $ createMigration $ migrate settable
      let queries = case m of
            Just (Right qs) -> intercalate ";" $ map (\(_, _, q) -> q) qs
            _ -> fail $ "Unexpected migration result: " ++ show m
          ref =
            if backendName proxy == "mysql"
              then "(`thirdTupleElement`) REFERENCES `test`.`sqlsettable`(`settable_id`)"
              else "(\"thirdTupleElement\") REFERENCES \"sqlsettable\"(\"settable_id\")"
          expectedNames = ["settable_id", "sqlsettable1", "firstTupleElement", "secondTupleElement", "thirdTupleElement", "someconstraint", "varchar(50)", ref]
          absent = filter (not . (`isInfixOf` queries)) expectedNames
      unless (null absent) $
        liftIO $ expectationFailure $ "Migration should contain " ++ show absent ++ ":\n" ++ queries
      migr settable
      k <- insert settable
      get k >>= \val' ->
        liftIO $
          val' `shouldBe` Just settable
      vals <- select $ Settable1Fld ==. "abc" &&. SettableTupleField ~> Tuple2_0Selector ==. (1 :: Int) &&. SettableTupleField ~> Tuple2_1Selector ~> Tuple2_0Selector ==. "qqq"
      liftIO $ vals `shouldBe` [settable]
      deleteBy singleKey -- test on delete cascade
      get k >>= \val' ->
        liftIO $
          val' `shouldBe` Nothing
      assertExc "Uniqueness constraint not enforced" $ insert settable >> insert settable

  it "creates triggers on delete for list" $
    runDbConnIO $ do
      migr (undefined :: Single (String, [[String]]))
      k <- insert (Single ("", [["abc", "def"]]) :: Single (String, [[String]]))
      listKey' <- queryRaw' "select \"single#val1\" from \"Single#Tuple2##String#List##List##String\" where id=?" [toPrimitivePersistValue k] >>= firstRow
      let listKey = head $ fromJust listKey'
      listsInsideListKeys <- queryRaw' "select value from \"List##List##String#values\" where id=?" [listKey] >>= streamToList
      deleteBy k
      -- test if the main list table and the associated values were deleted
      listMain <- queryRaw' "select * from \"List##List##String\" where id=?" [listKey] >>= firstRow
      liftIO $ listMain `shouldBe` Nothing
      listValues <- queryRaw' "select * from \"List##List##String#values\" where id=?" [listKey] >>= firstRow
      liftIO $ listValues `shouldBe` Nothing
      -- test if the ephemeral values associated with the list were deleted
      forM_ listsInsideListKeys $ \listsInsideListKey -> do
        sublist <- queryRaw' "select * from \"List##String\" where id=?" listsInsideListKey >>= firstRow
        liftIO $ sublist `shouldBe` Nothing

  it "creates triggers on update for list" $
    runDbConnIO $ do
      let val = Single [["abc", "def"]]
      migr val
      k <- insert val
      listKey' <- queryRaw' "select \"single\" from \"Single#List##List##String\" where id=?" [toPrimitivePersistValue k] >>= firstRow
      let listKey = head $ fromJust listKey'
      listsInsideListKeys <- queryRaw' "select value from \"List##List##String#values\" where id=?" [listKey] >>= streamToList
      replace k (Single [] :: Single [[String]])
      -- test if the main list table and the associated values were deleted
      listMain <- queryRaw' "select * from \"List##List##String\" where id=?" [listKey] >>= firstRow
      liftIO $ listMain `shouldBe` Nothing
      listValues <- queryRaw' "select * from \"List##List##String#values\" where id=?" [listKey] >>= firstRow
      liftIO $ listValues `shouldBe` Nothing
      -- test if the ephemeral values associated with the list were deleted
      forM_ listsInsideListKeys $ \listsInsideListKey -> do
        sublist <- queryRaw' "select * from \"List##String\" where id=?" listsInsideListKey >>= firstRow
        liftIO $ sublist `shouldBe` Nothing

  it "adds a new column" $
    runDbConnIO $ do
      migr (undefined :: Old.AddColumn)
      k <- insert $ Old.AddColumn 5
      migr (undefined :: New.AddColumn)
      k' <- toSinglePersistValue k >>= fromSinglePersistValue
      Just (New.AddColumn "new_column_default" 5) @=?? get k'

  it "adds a new unique constraint" $
    runDbConnIO $ do
      let val = Old.AddUniqueConstraint 5 6
      migr (undefined :: Old.AddUniqueConstraint)
      insert_ val
      migr (undefined :: New.AddUniqueConstraint)
      assertExc "Uniqueness constraint not enforced" $ insert val

  it "drops a unique constraint" $
    runDbConnIO $ do
      let val = Old.AddUniqueConstraint 5 6
      migr (undefined :: New.AddUniqueConstraint)
      insert_ val
      migr (undefined :: Old.AddUniqueConstraint)
      void $ insert val

  it "adds a new unique index" $
    runDbConnIO $ do
      let val = Old.AddUniqueIndex 5 6
      migr (undefined :: Old.AddUniqueIndex)
      insert_ val
      migr (undefined :: New.AddUniqueIndex)
      assertExc "Unique index not enforced" $ insert val

  it "drops a unique index" $
    runDbConnIO $ do
      let val = Old.AddUniqueIndex 5 6
      migr (undefined :: New.AddUniqueIndex)
      insert_ val
      migr (undefined :: Old.AddUniqueIndex)
      void $ insert val

  it "adds and drops NOT NULL" $
    runDbConnIO $ do
      let val = Old.AddNotNull Nothing
      let val2 = Old.AddNotNull $ Just "abc"
      migr (undefined :: New.AddNotNull)
      k <- insert val2
      migr (undefined :: Old.AddNotNull)
      insert val >>= deleteBy
      migr (undefined :: New.AddNotNull)
      val2' <- get k
      liftIO $ val2' `shouldBe` Just val2
      assertExc "Not null not enforced" $ insert val

  it "adds another constructor to a data type with several constructors" $
    runDbConnIO $ do
      migr (undefined :: Old.AddConstructorToMany)
      Old.AddConstructorToManyKey k1 <- insert $ Old.AddConstructorToMany1 1
      Old.AddConstructorToManyKey k2 <- insert $ Old.AddConstructorToMany2 "abc"
      migr (undefined :: New.AddConstructorToMany)
      k0 <- insert $ New.AddConstructorToMany0 5
      val1 <- get (New.AddConstructorToManyKey k1)
      liftIO $ val1 `shouldBe` Just (New.AddConstructorToMany1 1)
      val2 <- get (New.AddConstructorToManyKey k2)
      liftIO $ val2 `shouldBe` Just (New.AddConstructorToMany2 "abc")
      val0 <- get k0
      liftIO $ val0 `shouldBe` Just (New.AddConstructorToMany0 5)

  it "changes type of a column" $
    runDbConnIO $ do
      let val = Old.ChangeType True
          val2 = New.ChangeType "abc"
      migr val
      insert_ val
      migr val2
      Just val2 @=?? (insert val2 >>= get)

  it "works with long table names" $
    runDbConnIO $ do
      let val = Single [(Single [Single ""], 0 :: Int, [""], (), [""])]
      migr val
      Just val @=?? (insert val >>= get)

      let val2 = Single [([""], Single "", 0 :: Int)]
      migr val2
      m2 <- createMigration (migrate val2)
      executeMigration m2
      -- this might fail because the constraint names are too long. They constraints are created successfully, but with stripped names. Then during the second migration the stripped names differ from expected and this leads to migration errors.
      liftIO $ filter (/= Right []) (Map.elems m2) `shouldBe` []

  it "works with data types that have no fields" $
    runDbConnIO $ do
      let val = NoColumns
      migr val
      k1 <- insert val
      k2 <- insert val
      [k1, k2] @=?? (project (AutoKeyField :: AutoKeyField NoColumns c) $ CondEmpty `orderBy` [Asc AutoKeyField])

  it "works with data types that have no keys" $
    runDbConnIO $ do
      let val = NoKeys 1 2
      m <- fmap Map.elems $ createMigration $ migrate val
      let queries = concatMap (either (const "") (concatMap (\(_, _, q) -> q))) m
      when (" KEY " `isInfixOf` queries) $
        fail $ "Unexpected migration result: " ++ show m
      migr val
      [val] @=?? (insert val >> select CondEmpty)

  it "analyzes schema" $
    runDbConnIO $ do
      let val = Single (UniqueKeySample 1 2 (Just 3))
      migr val
      singleInfo <- analyzeTable (Nothing, persistName val)
      uniqueInfo <- analyzeTable (Nothing, persistName (undefined :: UniqueKeySample))
      let match (TableInfo cols1 uniqs1 refs1) (TableInfo cols2 uniqs2 refs2) =
            haveSameElems ((==) `on` \x -> x {colDefault = Nothing}) cols1 cols2
              && haveSameElems ((==) `on` \x -> x {uniqueDefName = Just ""}) uniqs1 uniqs2
              && haveSameElems (\(_, r1) (_, r2) -> ((==) `on` (snd . referencedTableName)) r1 r2 && (haveSameElems (==) `on` referencedColumns) r1 r2) refs1 refs2
          expectedSingleInfo =
            TableInfo
              [Column "id" False DbInt64 Nothing, Column "single#uniqueKey2" False DbInt64 Nothing, Column "single#uniqueKey3" True DbInt64 Nothing]
              [UniqueDef Nothing (UniquePrimary True) [Left "id"]]
              [(Nothing, Reference (Nothing, "UniqueKeySample") [("single#uniqueKey2", "uniqueKey2"), ("single#uniqueKey3", "uniqueKey3")] Nothing Nothing)]
          expectedUniqueInfo =
            TableInfo
              [Column "uniqueKey1" False DbInt64 Nothing, Column "uniqueKey2" False DbInt64 Nothing, Column "uniqueKey3" True DbInt64 Nothing]
              [UniqueDef Nothing UniqueConstraint [Left "uniqueKey1"], UniqueDef Nothing UniqueConstraint [Left "uniqueKey2", Left "uniqueKey3"], UniqueDef Nothing (UniquePrimary False) [Left "uniqueKey1", Left "uniqueKey2"]]
              []

      case singleInfo of
        Just t | match t expectedSingleInfo -> pure ()
        _ -> liftIO $ expectationFailure $ "Single does not match the expected schema: " ++ show singleInfo
      case uniqueInfo of
        Just t | match t expectedUniqueInfo -> pure ()
        _ -> liftIO $ expectationFailure $ "UniqueKeySample does not match the expected schema: " ++ show uniqueInfo

migrateAnotherSchemaSpec :: (PersistBackendConn conn, ExtractConnection cm conn, SchemaAnalyzer conn) => SpecWith cm
migrateAnotherSchemaSpec = do
  it "works with a foreign key to a table in another schema" $
    runDbConnIO $ do
      let val = InCurrentSchema Nothing
      migr val -- InAnotherSchema will be migrated automatically
      k <- insert val
      let val2 = InAnotherSchema (Just k)
      Just val2 @=?? (insert val2 >>= get)
