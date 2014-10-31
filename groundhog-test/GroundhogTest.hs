{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, RankNTypes, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, StandaloneDeriving, EmptyDataDecls, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module GroundhogTest (
      testSelect
    , testSelectDistinct
    , testCond
    , testArith
    , testProjectionSql
    , testNumber
    , testPersistSettings
    , testEmbedded
    , testInsert
    , testMaybe
    , testCount
    , testUpdate
    , testComparison
    , testEncoding
    , testDelete
    , testDeleteBy
    , testDeleteAll
    , testReplaceSingle
    , testReplaceMulti
    , testReplaceBy
    , testTuple
    , testTupleList
    , testMigrateAddColumnSingle
    , testMigrateAddUniqueConstraint
    , testMigrateDropUniqueConstraint
    , testMigrateAddUniqueIndex
    , testMigrateDropUniqueIndex
    , testMigrateAddDropNotNull
    , testMigrateAddConstructorToMany
    , testMigrateChangeType
    , testLongNames
    , testReference
    , testMaybeReference
    , testUniqueKey
    , testForeignKeyUnique
    , testProjection
    , testKeyNormalization
    , testAutoKeyField
    , testTime
    , testPrimitiveData
    , testNoColumns
    , testNoKeys
    , testMigrateOrphanConstructors
    , testSchemas
    , testFloating
    , testListTriggersOnDelete
    , testListTriggersOnUpdate
    , testSchemaAnalysis
#if WITH_SQLITE
    , testSchemaAnalysisSqlite
#endif
#if WITH_POSTGRESQL
    , testSchemaAnalysisPostgresql
    , testGeometry
    , testArrays
    , testSelectDistinctOn
    , testExpressionIndex
#endif
#if WITH_MYSQL
    , testSchemaAnalysisMySQL
#endif
) where

import qualified Control.Exception as E
import Control.Exception.Base (SomeException)
import Control.Monad (liftM, forM_, unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Migration
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Generic.Sql.Functions
import Database.Groundhog.TH
#if WITH_SQLITE
import Database.Groundhog.Sqlite
#endif
#if WITH_POSTGRESQL
import Database.Groundhog.Postgresql
import Database.Groundhog.Postgresql.Array hiding (all, any, append)
import qualified Database.Groundhog.Postgresql.Array as Arr
import Database.Groundhog.Postgresql.Geometry hiding ((>>), (&&))
import qualified Database.Groundhog.Postgresql.Geometry as Geo
#endif
#if WITH_MYSQL
import Database.Groundhog.MySQL
#endif
import Data.ByteString.Char8 (unpack)
import Data.Function (on)
import Data.Int
import Data.List (intercalate, isInfixOf, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.String.Utils as Utils
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Traversable as T
import Data.Word
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Migration.Old as Old
import qualified Migration.New as New
import qualified Test.HUnit as H
import Prelude hiding (catch)

data Number = Number {int :: Int, int8 :: Int8, word8 :: Word8, int16 :: Int16, word16 :: Word16, int32 :: Int32, word32 :: Word32, int64 :: Int64, word64 :: Word64} deriving (Eq, Show)
data MaybeContext a = MaybeContext (Maybe a) deriving (Eq, Show)
data Single a = Single {single :: a} deriving (Eq, Show)
data Multi a = First {first :: Int} | Second {second :: a} deriving (Eq, Show)
data Settable = Settable {settable1 :: String, settable2 :: Maybe (Key (Single String) BackendSpecific), settableTuple :: (Int, (String, Maybe Int64))}
data Keys = Keys {refDirect :: Single String, refKey :: DefaultKey (Single String), refDirectMaybe :: Maybe (Single String), refKeyMaybe :: Maybe (DefaultKey (Single String))}
data EmbeddedSample = EmbeddedSample {embedded1 :: String, embedded2 :: (Int, Int)} deriving (Eq, Show)
data UniqueKeySample = UniqueKeySample { uniqueKey1 :: Int, uniqueKey2 :: Int, uniqueKey3 :: Maybe Int } deriving (Eq, Show)
data InCurrentSchema = InCurrentSchema { inCurrentSchema :: Maybe (Key InAnotherSchema BackendSpecific) }
data InAnotherSchema = InAnotherSchema { inAnotherSchema :: Maybe (Key InCurrentSchema BackendSpecific) }
data EnumTest = Enum1 | Enum2 | Enum3 deriving (Eq, Show, Enum)
data ShowRead = ShowRead String Int deriving (Eq, Show, Read)
data NoColumns = NoColumns deriving (Eq, Show)
data NoKeys = NoKeys Int Int deriving (Eq, Show)
data ExpressionIndex = ExpressionIndex { expressionIndex :: Int } deriving (Eq, Show)

-- cannot use ordinary deriving because it runs before mkPersist and requires (Single String) to be an instance of PersistEntity
deriving instance Eq Keys
deriving instance Show Keys
deriving instance Eq Settable
deriving instance Show Settable
deriving instance Eq InCurrentSchema
deriving instance Show InCurrentSchema
deriving instance Eq InAnotherSchema
deriving instance Show InAnotherSchema

mkPersist defaultCodegenConfig [groundhog|
- entity: Number
- entity: MaybeContext
- entity: Single
- entity: Multi
- entity: Keys
- entity: Settable
  dbName: sqlsettable
  constructors:
    - name: Settable
      phantomName: SettableFooBarConstructor
      dbName: entity_db_name_is_used_instead
      keyDbName: settable_id
      fields:
        - name: settable1
          dbName: sqlsettable1
          type: varchar(50)
          exprName: Settable1Fld
        - name: settable2
          reference:
            onDelete: cascade
        - name: settableTuple
          embeddedType:
            - name: val0
              dbName: firstTupleElement
            - name: val1
              embeddedType:
                - name: val0
                  dbName: secondTupleElement
                - name: val1
                  dbName: thirdTupleElement
                  reference:
                    table: sqlsettable
                    columns: [settable_id]
              dbName: name
      uniques:
        - name: someconstraint
          fields: [settable1, settable2]
- embedded: EmbeddedSample
  fields:
    - name: embedded2
      embeddedType:
        - name: val0
          dbName: embeddedTuple0
        - name: val1
          dbName: embeddedTuple1
- entity: UniqueKeySample
  autoKey: null
  keys:
    - name: unique_key_one_column
    - name: unique_key_two_columns
      default: true
  constructors:
    - name: UniqueKeySample
      uniques:
        - name: unique_key_one_column
          fields: [uniqueKey1]
        - name: unique_key_two_columns
          fields: [uniqueKey2, uniqueKey3]
        - name: unique_primary
          type: primary
          fields: [uniqueKey1, uniqueKey2]
- entity: InCurrentSchema
- entity: InAnotherSchema
  schema: myschema
- primitive: EnumTest
  representation: enum
- primitive: ShowRead
  representation: showread # by default
- entity: NoColumns
- entity: NoKeys
  autoKey: null
- entity: ExpressionIndex
  constructors:
    - name: ExpressionIndex
      uniques:
        - name: Expression_index
          type: index
          fields: [{expr: "(abs(\"expressionIndex\") + 1)" }]
|]

data HoldsUniqueKey = HoldsUniqueKey { foreignUniqueKey :: Key UniqueKeySample (Unique Unique_key_one_column) } deriving (Eq, Show)

mkPersist defaultCodegenConfig [groundhog|
- entity: HoldsUniqueKey
  keys:
    - name: foreignUniqueKey
  constructors:
  - name: HoldsUniqueKey
    uniques:
      - name: foreignUniqueKey
        fields: [foreignUniqueKey]
|]

migr :: (PersistEntity v, PersistBackend m, MonadBaseControl IO m, MonadIO m) => v -> m ()
migr v = do
  m1 <- createMigration $ migrate v
  executeMigration m1
  m2 <- createMigration $ migrate v
  let afterMigration = filter (/= Right []) (Map.elems m2)
  liftIO $ unless (null afterMigration) $
    H.assertFailure $ ("first migration: " ++ show (Map.elems m1) ++ "\nsecond migration:" ++ show afterMigration)
    

(@=?) :: (Eq a, Show a, MonadBaseControl IO m, MonadIO m) => a -> a -> m ()
expected @=? actual = liftIO $ expected H.@=? actual

(@=??) :: (Eq a, Show a, MonadBaseControl IO m, MonadIO m) => a -> m a -> m ()
expected @=?? action = action >>= \actual -> expected @=? actual

testMigrateOrphanConstructors :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMigrateOrphanConstructors = do
  migr (undefined :: Multi String)
  executeRaw False "drop table \"Multi#String\"" []
  assertExc "Migration must fail in presence of orhpan constructor tables" $ migr (undefined :: Multi String)

testNumber :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testNumber = do
  migr (undefined :: Number)
  let minNumber = Number minBound minBound minBound minBound minBound minBound minBound minBound minBound
  let maxNumber = Number maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound
  Just minNumber @=?? (insert minNumber >>= get)
  Just maxNumber @=?? (insert maxNumber >>= get)

testPersistSettings :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testPersistSettings = do
  proxy <- phantomDb
  let val = Single "def"
  migr val
  singleKey <- insert val
  let settable = Settable "abc" (Just singleKey) (1, ("qqq", Nothing))
  m <- fmap (Map.lookup "entity sqlsettable") $ createMigration $ migrate settable
  let queries = case m of
        Just (Right qs) -> intercalate ";" $ map (\(_, _, q) -> q) qs
        t -> fail $ "Unexpected migration result: " ++ show m
      ref = if backendName proxy == "mysql"
        then "(`thirdTupleElement`) REFERENCES `test`.`sqlsettable`(`settable_id`)"
        else "(\"thirdTupleElement\") REFERENCES \"sqlsettable\"(\"settable_id\")"
      expectedNames = ["settable_id", "sqlsettable1", "firstTupleElement", "secondTupleElement", "thirdTupleElement", "someconstraint", "varchar(50)", ref]
      absent = filter (not . (`isInfixOf` queries)) expectedNames
  liftIO $ null absent H.@? ("Migration should contain " ++ show absent ++ ":\n" ++ queries)
  migr settable
  k <- insert settable
  Just settable @=?? get k
  vals <- select $ Settable1Fld ==. "abc" &&. SettableTupleField ~> Tuple2_0Selector ==. (1 :: Int) &&. SettableTupleField ~> Tuple2_1Selector ~> Tuple2_0Selector ==. "qqq"
  [settable] @=? vals
  deleteBy singleKey -- test on delete cascade
  Nothing @=?? get k
  assertExc "Uniqueness constraint not enforced" $ insert settable >> insert settable

testEmbedded :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testEmbedded = do
  let val1 = Single (EmbeddedSample "abc" (5, 6))
  migr val1
  k1 <- insert val1
  val1' <- get k1
  Just val1 @=? val1'
  vals <- select $ SingleField ~> Embedded1Selector ==. "abc" &&. SingleField ~> Embedded2Selector ==. (5, 6) &&. SingleField ~> Embedded2Selector ~> Tuple2_0Selector ==. (5 :: Int)
  [val1] @=? vals
  let val2 = Single (EmbeddedSample "abc" (5, 6), "def")
  migr val2
  k2 <- insert val2
  val2' <- get k2
  Just val2 @=? val2'

testInsert :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testInsert = do
  let val = Single "abc"
  let multi = Second "abc"
  migr val
  migr multi
  Just val @=?? (insert val >>= get)
  Just multi @=?? (insert multi >>= get)
  insert_ val
  insert_ multi
  [val, val] @=?? liftM (map snd) selectAll
  [multi, multi] @=?? liftM (map snd) selectAll

-- There is a weird bug in GHC 7.4.1 which causes program to hang if there is a type class constraint which is not used. See ticket 7126. It may arise with maybes
-- instance (PersistField a, NeverNull a) => PersistField (Maybe a) where -- OK
-- instance (SinglePersistField a, NeverNull a) => PersistField (Maybe a) where -- HANGS
testMaybe :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMaybe = do
  let val = Single (Just "abc")
  migr val
  Just val @=?? (insert val >>= get)

testSelect :: (PersistBackend m, MonadBaseControl IO m, MonadIO m, db ~ PhantomDb m, SqlDb db) => m ()
testSelect = do
  migr (undefined :: Single (Int, String))
  let val1 = Single (5 :: Int, "abc")
  let val2 = Single (7 :: Int, "DEF")
  let val3 = Single (11 :: Int, "ghc")
  insert val1
  insert val2
  insert val3
  [val3] @=?? (select $ (SingleField ~> Tuple2_0Selector >. (5 :: Int)) `orderBy` [Asc (SingleField ~> Tuple2_1Selector)] `offsetBy` 1)
  [val2] @=?? (select $ (SingleField ~> Tuple2_0Selector >. (5 :: Int)) `orderBy` [Asc (SingleField ~> Tuple2_1Selector)] `limitTo` 1)
  [val2] @=?? (select $ (SingleField >=. (6 :: Int, "something") &&. SingleField ~> Tuple2_1Selector <. "ghc") `limitTo` 1)
  [val3] @=?? (select $ liftExpr (SingleField ~> Tuple2_0Selector) + 1 >. (10 :: Int))
  [val2] @=?? (select $ (SingleField ~> Tuple2_1Selector) `like` "%E%")
  [val2] @=?? (select $ lower (SingleField ~> Tuple2_1Selector) ==. "def")
  [val1, val2] @=?? (select $ ((SingleField ~> Tuple2_0Selector) `in_` [7 :: Int, 5]) `orderBy` [Asc (SingleField ~> Tuple2_0Selector)])
  [val1] @=?? (select $ CondEmpty `orderBy` [Asc SingleField] `limitTo` 1)
  [val1] @=?? (select $ CondEmpty `orderBy` [Asc SingleField, Desc SingleField] `limitTo` 1)
  ([] :: [Single (Int, String)]) @=?? (select $ Not CondEmpty)

testSelectDistinct :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testSelectDistinct = do
  let val1 = Single (5 :: Int, "abc")
      val2 = Single (6 :: Int, "def")
  migr val1
  insert val1
  insert val1
  insert val2
  [val1, val2] @=?? (select $ distinct $ CondEmpty `orderBy` [Asc $ SingleField ~> Tuple2_0Selector])

testArith :: (PersistBackend m, MonadBaseControl IO m, MonadIO m, db ~ PhantomDb m, SqlDb db) => m ()
testArith = do
  let val = Single (1 :: Int)
  migr val
  k <- insert val
  [(4, -4, 4)] @=?? project ((liftExpr SingleField + 1) * 2, liftExpr SingleField - 5, abs $ liftExpr SingleField - 5) (AutoKeyField ==. k)
  [(1, 0, -1)] @=?? project (signum $ liftExpr SingleField, signum $ liftExpr SingleField - 1, signum $ liftExpr SingleField - 10) (AutoKeyField ==. k)
  [(-6, -1)] @=?? project (quotRem (liftExpr SingleField - 20) 3) (AutoKeyField ==. k)
  [(-7, 2)] @=?? project (divMod (liftExpr SingleField - 20) 3) (AutoKeyField ==. k)

testCond :: forall m db . (PersistBackend m, MonadBaseControl IO m, MonadIO m, db ~ PhantomDb m, SqlDb db) => m ()
testCond = do
  proxy <- phantomDb
  let rend :: forall r . Cond (PhantomDb m) r -> Maybe (RenderS (PhantomDb m) r)
      rend = renderCond $ RenderConfig id
  let (===) :: forall r a. (PrimitivePersistField a) => (String, [a]) -> Cond (PhantomDb m) r -> m ()
      (query, vals) === cond = let
            Just (RenderS q v) = rend cond
            equals = case backendName proxy of
               "sqlite" -> " IS "
               "postgresql" -> " IS NOT DISTINCT FROM "
               "mysql" -> "<=>"
               _ -> "="
            query' = Utils.replace "=" equals query
         in (query', map (toPrimitivePersistValue proxy) vals) @=? (unpack $ fromUtf8 $ q, v [])

  let intField f = f `asTypeOf` (undefined :: Field (Single (Int, Int)) c a)
      intNum = fromInteger :: Integer -> Expr db r Int
  -- should cover all cases of renderCond comparison rendering
  ("single#val0=? AND single#val1=?", ["abc", "def"]) === (SingleField ==. ("abc", "def"))
  ("single#val0=single#val1", [] :: [Int]) === (intField SingleField ~> Tuple2_0Selector ==. SingleField ~> Tuple2_1Selector)
  ("single#val1=single#val0*(?+single#val0)", [5 :: Int]) === (intField SingleField ~> Tuple2_1Selector ==. liftExpr (SingleField ~> Tuple2_0Selector) * (5 + liftExpr (SingleField ~> Tuple2_0Selector)))

  ("?=? AND ?=?", [1, 2, 3, 4 :: Int]) === ((1 :: Int, 3 :: Int) ==. (2 :: Int, 4 :: Int) &&. SingleField ==. ()) -- SingleField ==. () is required to replace Any with a PersistEntity instance
  ("?<? OR ?<?", [1, 2, 3, 4 :: Int]) === ((1 :: Int, 3 :: Int) <. (2 :: Int, 4 :: Int) &&. SingleField ==. ())
  ("?=single#val0 AND ?=single#val1", [1, 2 :: Int]) === ((1 :: Int, 2 :: Int) ==. SingleField)
  ("?=single+?*?", [1, 2, 3 :: Int]) === ((1 :: Int) ==. liftExpr SingleField + 2 * 3)

--  ("?-single=?", [1, 2 :: Int]) === (1 - liftExpr SingleField ==. (2 :: Int))
  ("?*single>single", [1 :: Int]) === (intNum 1 * liftExpr SingleField >. SingleField)
--  ("?+single>=single-?", [1, 2 :: Int]) === (intNum 1 + liftExpr SingleField >=. liftExpr SingleField - 2)
  
  -- test parentheses
  ("NOT (NOT single=? OR ?=? AND ?=?)", [0, 1, 2, 3, 4 :: Int]) === (Not $ Not (SingleField ==. (0 :: Int)) ||. (1 :: Int, 3 :: Int) ==. (2 :: Int, 4 :: Int))
  ("single=? AND (?<? OR ?<?)", [0, 1, 2, 3, 4 :: Int]) === (SingleField ==. (0 :: Int) &&. (1 :: Int, 3 :: Int) <. (2 :: Int, 4 :: Int))
  ("NOT (single=? AND (single=single OR single<single))", [0 :: Int]) === (Not $ SingleField ==. (0 :: Int) &&. (SingleField ==. SingleField ||. SingleField <. SingleField))
  
  -- test empty conditions
  ("single#val0=? AND single#val1=?", ["abc", "def"]) === (SingleField ==. ("abc", "def") &&. (() ==. () ||. ((), ()) <. ((), ())))
  ("single#val0=? AND single#val1=?", ["abc", "def"]) === ((() ==. () ||. ((), ()) <. ((), ())) &&. SingleField ==. ("abc", "def"))

  -- test conditions used as expressions
  ("(?=?)=(?=?)", [1, 1, 0, 0 :: Int]) === (((1 :: Int) ==. (1 :: Int)) ==. ((0 :: Int) ==. (0 :: Int)))

testCount :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testCount = do
  migr (undefined :: Multi String)
  insert (First 0 :: Multi String)
  insert (Second "abc")
  num <- countAll (undefined :: Multi String)
  2 @=? num
  num2 <- count $ SecondField ==. "abc"
  1 @=? num2
  migr (undefined :: Single String)
  insert $ Single "abc"
  num3 <- count (SingleField ==. "abc")
  1 @=? num3

testUpdate :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testUpdate = do
  let val = Single ("abc", "def")
  migr val
  k <- insert val
  -- update columns using embedded data structure
  update [SingleField =. ("ghc", "qqq")] (SingleField ~> Tuple2_0Selector ==. "abc")
  val1 <- get k
  Just (Single ("ghc", "qqq")) @=? val1
  -- update columns to the initial values using embedded data structure subfields
  update [SingleField ~> Tuple2_0Selector =. "abc", SingleField ~> Tuple2_1Selector =. "def"] (SingleField ==. ("ghc", "qqq"))
  val2 <- get k
  Just val @=? val2

testComparison :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testComparison = do
  let val1 = Single (1 :: Int)
  let val2 = Single (2 :: Int)
  migr val1
  k1 <- insert val1
  k2 <- insert val2
  result1 <- select $ SingleField ==. (1 :: Int)
  [val1] @=? result1
  result2 <- select $ SingleField /=. (1 :: Int)
  [val2] @=? result2
  result3 <- select $ SingleField <.  (2 :: Int)
  [val1] @=? result3
  result4 <- select $ SingleField >. (1 :: Int)
  [val2] @=? result4
  result5 <- select $ SingleField >=. (2 :: Int)
  [val2] @=? result5
  result6 <- select $ SingleField <=. (1 :: Int)
  [val1] @=? result6

testEncoding :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testEncoding = do
  let val = Single $ "\x0001\x0081\x0801\x1001" ++ ['\1'..'\255']
  migr val
  k <- insert val
  val' <- get k
  Just val @=? val'

testTuple :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testTuple = do
  let val = Single ("abc", ("def", 5 :: Int))
  migr val
  k <- insert val
  val' <- get k
  Just val @=? val'

testTupleList :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testTupleList = do
  let val = Single [("abc", 4 :: Int), ("def", 5)]
  migr val
  k <- insert val
  val' <- get k
  Just val @=? val'
  
testListTriggersOnDelete :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testListTriggersOnDelete = do
  migr (undefined :: Single (String, [[String]]))
  proxy <- phantomDb
  k <- insert (Single ("", [["abc", "def"]]) :: Single (String, [[String]]))
  Just [listKey] <- queryRaw' "select \"single#val1\" from \"Single#Tuple2##String#List##List##String\" where id=?" [toPrimitivePersistValue proxy k] firstRow
  listsInsideListKeys <- queryRaw' "select value from \"List##List##String#values\" where id=?" [listKey] $ mapAllRows return
  deleteBy k
  -- test if the main list table and the associated values were deleted
  listMain <- queryRaw' "select * from \"List##List##String\" where id=?" [listKey] firstRow
  Nothing @=? listMain
  listValues <- queryRaw' "select * from \"List##List##String#values\" where id=?" [listKey] firstRow
  Nothing @=? listValues
  -- test if the ephemeral values associated with the list were deleted
  forM_ listsInsideListKeys $ \listsInsideListKey -> do
    sublist <- queryRaw' "select * from \"List##String\" where id=?" listsInsideListKey firstRow
    Nothing @=? sublist

testListTriggersOnUpdate :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testListTriggersOnUpdate = do
  let val = Single [["abc", "def"]]
  migr val
  proxy <- phantomDb
  k <- insert val
  Just [listKey] <- queryRaw' "select \"single\" from \"Single#List##List##String\" where id=?" [toPrimitivePersistValue proxy k] firstRow
  listsInsideListKeys <- queryRaw' "select value from \"List##List##String#values\" where id=?" [listKey] $ mapAllRows return
  replace k (Single [] :: Single [[String]])
  -- test if the main list table and the associated values were deleted
  listMain <- queryRaw' "select * from \"List##List##String\" where id=?" [listKey] firstRow
  Nothing @=? listMain
  listValues <- queryRaw' "select * from \"List##List##String#values\" where id=?" [listKey] firstRow
  Nothing @=? listValues
  -- test if the ephemeral values associated with the list were deleted
  forM_ listsInsideListKeys $ \listsInsideListKey -> do
    sublist <- queryRaw' "select * from \"List##String\" where id=?" listsInsideListKey firstRow
    Nothing @=? sublist

testDelete :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testDelete = do
  migr (undefined :: Multi String)
  proxy <- phantomDb
  k <- insert $ Second "abc"
  delete $ SecondField ==. "abc"
  Nothing @=?? queryRaw' "SELECT * FROM \"Multi#String\" WHERE id=?" [toPrimitivePersistValue proxy k] firstRow
  Nothing @=?? queryRaw' "SELECT * FROM \"Multi#String#Second\" WHERE id=?" [toPrimitivePersistValue proxy k] firstRow

testDeleteBy :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testDeleteBy = do
  migr (undefined :: Multi String)
  proxy <- phantomDb
  k <- insert $ Second "abc"
  deleteBy k
  Nothing @=?? queryRaw' "SELECT * FROM \"Multi#String\" WHERE id=?" [toPrimitivePersistValue proxy k] firstRow
  Nothing @=?? queryRaw' "SELECT * FROM \"Multi#String#Second\" WHERE id=?" [toPrimitivePersistValue proxy k] firstRow

testDeleteAll :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testDeleteAll = do
  let val = Second "abc"
  migr val
  proxy <- phantomDb
  insert val
  deleteAll val
  Nothing @=?? queryRaw' "SELECT * FROM \"Multi#String\"" [] firstRow
  Nothing @=?? queryRaw' "SELECT * FROM \"Multi#String#Second\"" [] firstRow

testReplaceMulti :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testReplaceMulti = do
  migr (undefined :: Single (Multi String))
  proxy <- phantomDb
  -- we need Single to test that referenced value can be replaced
  k <- insert $ Single (Second "abc")
  Just [valueKey'] <- queryRaw' "SELECT \"single\" FROM \"Single#Multi#String\" WHERE id=?" [toPrimitivePersistValue proxy k] firstRow
  let valueKey = fromPrimitivePersistValue proxy valueKey'

  replace valueKey (Second "def")
  replaced <- get valueKey
  Just (Second "def") @=? replaced

  replace valueKey (First 5)
  replaced <- get valueKey
  Just (First 5) @=? replaced
  oldConstructor <- queryRaw' "SELECT * FROM \"Multi#String#Second\" WHERE id=?" [toPrimitivePersistValue proxy valueKey] firstRow
  Nothing @=? oldConstructor

testReplaceSingle :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testReplaceSingle = do
  -- we need Single to test that referenced value can be replaced
  let val = Single (Single "abc")
  migr val
  proxy <- phantomDb
  k <- insert val
  Just [valueKey'] <- queryRaw' "SELECT \"single\" FROM \"Single#Single#String\" WHERE id=?" [toPrimitivePersistValue proxy k] firstRow
  let valueKey = fromPrimitivePersistValue proxy valueKey'
  replace valueKey (Single "def")
  replaced <- get valueKey
  Just (Single "def") @=? replaced

testReplaceBy :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testReplaceBy = do
  let val = UniqueKeySample 1 2 (Just 3)
      replaced = UniqueKeySample 1 3 Nothing
  migr val
  k <- insert val
  replaceBy Unique_key_one_column replaced
  Just replaced @=?? getBy (Unique_key_one_columnKey 1)

testMigrateAddColumnSingle :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMigrateAddColumnSingle = do
  migr (undefined :: Old.AddColumn)
  k <- insert $ Old.AddColumn 5
  migr (undefined :: New.AddColumn)
  k' <- toSinglePersistValue k >>= fromSinglePersistValue
  Just (New.AddColumn "new_column_default" 5) @=?? get k'

testMigrateAddUniqueConstraint :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMigrateAddUniqueConstraint = do
  let val = Old.AddUniqueConstraint 5 6
  migr (undefined :: Old.AddUniqueConstraint)
  insert val
  migr (undefined :: New.AddUniqueConstraint)
  assertExc "Uniqueness constraint not enforced" $ insert val
  return ()

testMigrateDropUniqueConstraint :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMigrateDropUniqueConstraint = do
  let val = Old.AddUniqueConstraint 5 6
  migr (undefined :: New.AddUniqueConstraint)
  insert val
  migr (undefined :: Old.AddUniqueConstraint)
  insert val
  return ()

testMigrateAddUniqueIndex :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMigrateAddUniqueIndex = do
  let val = Old.AddUniqueIndex 5 6
  migr (undefined :: Old.AddUniqueIndex)
  insert val
  migr (undefined :: New.AddUniqueIndex)
  assertExc "Unique index not enforced" $ insert val
  return ()

testMigrateDropUniqueIndex :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMigrateDropUniqueIndex = do
  let val = Old.AddUniqueIndex 5 6
  migr (undefined :: New.AddUniqueIndex)
  insert val
  migr (undefined :: Old.AddUniqueIndex)
  insert val
  return ()

testMigrateAddDropNotNull :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMigrateAddDropNotNull = do
  let val = Old.AddNotNull Nothing
  let val2 = Old.AddNotNull $ Just "abc"
  migr (undefined :: New.AddNotNull)
  k <- insert val2
  migr (undefined :: Old.AddNotNull)
  insert val >>= deleteBy
  migr (undefined :: New.AddNotNull)
  val2' <- get k
  Just val2 @=? val2'
  assertExc "Not null not enforced" $ insert val

testMigrateAddConstructorToMany :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMigrateAddConstructorToMany = do
  migr (undefined :: Old.AddConstructorToMany)
  Old.AddConstructorToManyKey k1 <- insert $ Old.AddConstructorToMany1 1
  Old.AddConstructorToManyKey k2 <- insert $ Old.AddConstructorToMany2 "abc"
  migr (undefined :: New.AddConstructorToMany)
  k0 <- insert $ New.AddConstructorToMany0 5
  val1 <- get (New.AddConstructorToManyKey k1)
  Just (New.AddConstructorToMany1 1) @=? val1
  val2 <- get (New.AddConstructorToManyKey k2)
  Just (New.AddConstructorToMany2 "abc") @=? val2
  val0 <- get k0
  Just (New.AddConstructorToMany0 5) @=? val0

testMigrateChangeType :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMigrateChangeType = do
  let val = Old.ChangeType True
      val2 = New.ChangeType "abc"
  migr val
  insert val
  migr val2
  Just val2 @=?? (insert val2 >>= get)

testLongNames :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testLongNames = do
  let val = Single [(Single [Single ""], 0 :: Int, [""], (), [""])]
  migr val
  Just val @=?? (insert val >>= get)

  let val2 = Single [([""], Single "", 0 :: Int)]
  migr val2
  m2 <- createMigration (migrate val2)
  executeMigration m2
  -- this might fail because the constraint names are too long. They constraints are created successfully, but with stripped names. Then during the second migration the stripped names differ from expected and this leads to migration errors.
  [] @=? filter (/= Right []) (Map.elems m2)

testReference :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testReference = do
  migr (undefined :: Single (Key (Single String) BackendSpecific))
  k <- insert $ Single "abc"
  insert $ Single k
  assertExc "Foreign key must prevent deletion" $ deleteBy k

testMaybeReference :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMaybeReference = do
  migr (undefined :: Single (Maybe (Key (Single String) BackendSpecific)))
  k <- insert $ Single "abc"
  insert $ Single $ Just k
  assertExc "Foreign key must prevent deletion" $ deleteBy k

testUniqueKey :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testUniqueKey = do
  let uVal = UniqueKeySample 1 2 (Just 3)
  let uKey = Unique_key_two_columnsKey 2 (Just 3)
  let val = Single uVal
  migr val
  k <- insert val
  Just val @=?? get k
  [uVal] @=?? select (Unique_key_two_columns ==. uKey)
  Left () @=?? insertByAll uVal
  -- check that constraints with nulls can be repeated
  insert $ UniqueKeySample 2 2 Nothing
  insert $ UniqueKeySample 3 2 Nothing
  insertByAll $ UniqueKeySample 4 2 Nothing
  3 @=?? count (Unique_key_two_columns ==. Unique_key_two_columnsKey 2 Nothing)
  return ()

testForeignKeyUnique :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testForeignKeyUnique = do
  let uVal = UniqueKeySample 1 2 (Just 3)
  let val = HoldsUniqueKey (extractUnique uVal)
  migr val
  insert uVal
  insert val
  return ()

testProjection :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testProjection = do
  let val = Single ("abc", 5 :: Int)
  migr val
  k <- insert val
  result <- project (AutoKeyField, SingleConstructor, SingleField, SingleField ~> Tuple2_1Selector, SingleField ==. SingleField) ("" ==. "")
  [(k, val, ("abc", 5 :: Int), 5 :: Int, True)] @=? result
  let uVal = UniqueKeySample 1 2 (Just 3)
  migr uVal
  insert uVal
  result2 <- project Unique_key_two_columns ("" ==. "")
  [extractUnique uVal] @=? result2

testProjectionSql :: (PersistBackend m, MonadBaseControl IO m, MonadIO m, db ~ PhantomDb m, SqlDb db) => m ()
testProjectionSql = do
  let val = Single ("abc", 5 :: Int)
  migr val
  insert val
  result <- project ("hello " `append` (upper $ SingleField ~> Tuple2_0Selector), liftExpr (SingleField ~> Tuple2_1Selector) + 1) (() ==. ())
  [("hello ABC", 6 :: Int)] @=? result

testKeyNormalization :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testKeyNormalization = do
  let val = undefined :: Single (Key (Single String) BackendSpecific)
  migr val
  let val1 = Single "abc"
  SingleKey k <- insert val1
  (kString :: String) <- fromSinglePersistValue k
  val' <- toSinglePersistValue kString >>= get . SingleKey
  Just val1 @=? val'

testAutoKeyField :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testAutoKeyField = do
  let val = Single "abc"
  migr val
  k <- insert val
  result <- select $ AutoKeyField ==. k
  [val] @=? result

-- This test must just compile
testKeys :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testKeys = do
  migr (undefined :: Keys)
  k <- insert $ Single ""
  select $ RefDirectField ==. k ||. RefKeyField ==. k ||. RefDirectMaybeField ==. Just k ||. RefKeyMaybeField ==. Just k
  select $ AutoKeyField ==. k
  return ()

instance Eq Time.ZonedTime where
  a == b = show a == show b

testTime :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testTime = do
  posixTime <- liftIO Time.getPOSIXTime
  let utcTime = Time.posixSecondsToUTCTime $ fromInteger $ truncate posixTime -- round to seconds
  let dayTime = Time.timeToTimeOfDay $ Time.utctDayTime utcTime
  let day = Time.utctDay utcTime
  timeZone <- liftIO Time.getCurrentTimeZone
  let zonedTime = Time.utcToZonedTime (timeZone {Time.timeZoneName = ""}) utcTime
  let val = Single (utcTime, dayTime, day, zonedTime)
  migr val
  k <- insert val
  Just val @=?? get k

testPrimitiveData :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testPrimitiveData = do
  let val = Single (Enum2, ShowRead "abc" 42)
  migr val
  Just val @=?? (insert val >>= get)

testNoColumns :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testNoColumns = do
  let val = NoColumns
  migr val
  k1 <- insert val
  k2 <- insert val
  [k1, k2] @=?? (project (AutoKeyField :: AutoKeyField NoColumns c) $ CondEmpty `orderBy` [Asc AutoKeyField])

testNoKeys :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testNoKeys = do
  let val = NoKeys 1 2
  m <- fmap Map.elems $ createMigration $ migrate val
  let queries = concat $ map (either (const "") (concat . map (\(_, _, q) -> q))) m
  if " KEY " `isInfixOf` queries
    then fail $ "Unexpected migration result: " ++ show m
    else return ()
  migr val
  [val] @=?? (insert val >> select CondEmpty)

testSchemas :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testSchemas = do
  let val = InCurrentSchema Nothing
  migr val -- InAnotherSchema will be migrated automatically
  k <- insert val
  let val2 = InAnotherSchema (Just k)
  Just val2 @=?? (insert val2 >>= get)

testFloating :: (PersistBackend m, MonadBaseControl IO m, MonadIO m, db ~ PhantomDb m, FloatingSqlDb db) => m ()
testFloating = do
  let val = Single (pi :: Double)
  migr val
  k <- insert val
  let expected @=??~ expr = do
        actual <- fmap head $ project expr $ AutoKeyField ==. k
        liftIO $ unless (abs (expected - actual) < 0.0001) $ expected @=? actual
  180 @=??~ degrees SingleField
  pi  @=??~ radians (degrees SingleField)
  0   @=??~ sin (liftExpr SingleField)
  (-1)@=??~ cos (liftExpr SingleField)
  1   @=??~ tan (liftExpr SingleField / 4)
  0   @=??~ cot (liftExpr SingleField / 2)
  (pi/2) @=??~ asin (sin $ liftExpr SingleField / 2)
  pi  @=??~ acos (cos $ liftExpr SingleField)
  exp 2   @=??~ exp 2
  sqrt pi @=??~ sqrt (liftExpr SingleField)
  exp pi @=??~ exp (liftExpr SingleField)
  (pi ** 2) @=??~ (liftExpr SingleField ** 2)
  log pi @=??~ log (liftExpr SingleField)
  logBase 3 pi @=??~ logBase 3 (liftExpr SingleField)

  sinh pi @=??~ sinh (liftExpr SingleField)
  tanh pi @=??~ tanh (liftExpr SingleField)
  cosh pi @=??~ cosh (liftExpr SingleField)
  asinh pi @=??~ asinh (liftExpr SingleField)
  atanh (pi / 4) @=??~ atanh (liftExpr SingleField / 4)
  acosh (pi) @=??~ acosh (liftExpr SingleField)

testSchemaAnalysis :: (PersistBackend m, MonadBaseControl IO m, MonadIO m, SchemaAnalyzer m) => m ()
testSchemaAnalysis = do
  let val = Single (UniqueKeySample 1 2 (Just 3))
  migr val
  singleInfo <- analyzeTable (Nothing, persistName val)
  uniqueInfo <- analyzeTable (Nothing, persistName (undefined :: UniqueKeySample))
  let match (TableInfo cols1 uniqs1 refs1) (TableInfo cols2 uniqs2 refs2) =
           haveSameElems ((==) `on` \x -> x {colDefault = Nothing}) cols1 cols2
        && haveSameElems ((==) `on` \x -> x {uniqueDefName = Just ""}) uniqs1 uniqs2
        && haveSameElems (\(_, r1) (_, r2) -> ((==) `on` (snd . referencedTableName)) r1 r2 && (haveSameElems (==) `on` referencedColumns) r1 r2 ) refs1 refs2
      expectedSingleInfo = TableInfo [Column "id" False DbInt64 Nothing, Column "single#uniqueKey2" False DbInt64 Nothing, Column "single#uniqueKey3" True DbInt64 Nothing]
        [UniqueDef Nothing (UniquePrimary True) [Left "id"]]
        [(Nothing, Reference (Nothing, "UniqueKeySample") [("single#uniqueKey2","uniqueKey2"), ("single#uniqueKey3","uniqueKey3")] Nothing Nothing)]
      expectedUniqueInfo = TableInfo [Column "uniqueKey1" False DbInt64 Nothing, Column "uniqueKey2" False DbInt64 Nothing, Column "uniqueKey3" True DbInt64 Nothing]
        [UniqueDef Nothing UniqueConstraint [Left "uniqueKey1"], UniqueDef Nothing UniqueConstraint [Left "uniqueKey2",Left "uniqueKey3"], UniqueDef Nothing (UniquePrimary False) [Left "uniqueKey1", Left "uniqueKey2"]]
        []

  case singleInfo of
    Just t | match t expectedSingleInfo -> return ()
    _ -> liftIO $ H.assertFailure $ "Single does not match the expected schema: " ++ show singleInfo
  case uniqueInfo of
    Just t | match t expectedUniqueInfo -> return ()
    _ -> liftIO $ H.assertFailure $ "UniqueKeySample does not match the expected schema: " ++ show uniqueInfo

#if WITH_SQLITE
testSchemaAnalysisSqlite :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist Sqlite m ()
testSchemaAnalysisSqlite = do
  let val = Single (Single "abc")
  migr val
  let action = "select * from \"Single#String\";"
  executeRaw False ("CREATE TRIGGER \"myTrigger\" AFTER DELETE ON \"Single#String\" FOR EACH ROW BEGIN " ++ action ++ " END") []
  ["Single#Single#String", "Single#String"] @=?? liftM sort (listTables Nothing)
  ["myTrigger"] @=?? liftM sort (listTableTriggers (Nothing, "Single#String"))
  sql <- analyzeTrigger (Nothing, "myTrigger")
  let sql' = maybe (error "No trigger found") id sql
  liftIO $ action `isInfixOf` sql' H.@? "Trigger does not contain action statement"
#endif

-- TODO: write test which inserts data before adding new columns

firstRow :: Monad m => RowPopper m -> m (Maybe [PersistValue])
firstRow = id

#if WITH_POSTGRESQL
testGeometry :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist Postgresql m ()
testGeometry = do
  let p = Point 1.2 3.45
  let val1 = Single (p, Lseg p p, Box p p, Circle p 6.7)
  let val2 = Single (OpenPath [p], ClosedPath [p], OpenPath [p, p], ClosedPath [p, p], (Polygon [p], Polygon [p, p]))
  migr val1
  k1 <- insert val1
  val1' <- get k1
  Just val1 @=? val1'
  migr val2
  k2 <- insert val2
  val2' <- get k2
  Just val2 @=? val2'

testArrays :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist Postgresql m ()
testArrays = do
  let val = Single (Array [1 :: Int, 2, 3])
  migr val
  k <- insert val
  val' <- get k
  Just val @=? val'
  -- test operators
  let int :: Int -> Int
      int = id
  [2] @=?? project (SingleField ! int 2) (AutoKeyField ==. k)
  [Array [2, 3]] @=?? project (SingleField !: (int 2, int 3)) (AutoKeyField ==. k)
  [Array [1..4]] @=?? project (SingleField `Arr.append` explicitType (int 4)) (AutoKeyField ==. k)
  [Array [0..3]] @=?? project (explicitType (int 0) `Arr.prepend` SingleField) (AutoKeyField ==. k)
  [Array [1..6]] @=?? project (SingleField `arrayCat` Array [int 4..6]) (AutoKeyField ==. k)
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
  [k] @=?? project AutoKeyField (AutoKeyField ==. k &&. Array [int 3..10] `overlaps` SingleField)
  -- test escaping/unescaping
  let myString = ['\1'..'\255'] :: String
  let val2 = Single (Array[myString], Array[Array[myString]])
  migr val2
  Just val2 @=?? (insert val2 >>= get)

testSchemaAnalysisPostgresql :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist Postgresql m ()
testSchemaAnalysisPostgresql = do
  let val = Single (Single "abc")
  migr val
  let action = "EXECUTE PROCEDURE \"myFunction\"()"
  executeRaw False "CREATE OR REPLACE FUNCTION \"myFunction\"() RETURNS trigger AS $$ BEGIN RETURN NEW;END; $$ LANGUAGE plpgsql" []
  executeRaw False ("CREATE TRIGGER \"myTrigger\" AFTER DELETE ON \"Single#String\" FOR EACH ROW " ++ action) []
  ["Single#Single#String", "Single#String"] @=?? liftM sort (listTables Nothing)
  ["myTrigger"] @=?? liftM sort (listTableTriggers (Nothing, "Single#String"))
  trigSql <- analyzeTrigger (Nothing, "myTrigger")
  let trigSql' = maybe (error "No trigger found") id trigSql
  liftIO $ action `isInfixOf` trigSql' H.@? "Trigger does not contain action statement"
  func <- analyzeFunction (Nothing, "myFunction")
  let (args, ret, body) = fromMaybe (error "No function found") func
  Just [] @=? args
  Just (DbOther (OtherTypeDef [Left "trigger"])) @=? ret
  liftIO $ "RETURN NEW;" `isInfixOf` body H.@? "Function does not contain action statement"

testSelectDistinctOn :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist Postgresql m ()
testSelectDistinctOn = do
  let val1 = Single (5 :: Int, "abc")
      val2 = Single (6 :: Int, "123")
  migr val1
  insert val1
  insert $ Single (5 :: Int, "def")
  insert val2
  [val1, val2] @=?? (select $ CondEmpty `distinctOn` (SingleField ~> Tuple2_0Selector) `orderBy` [Asc $ SingleField ~> Tuple2_0Selector, Asc $ SingleField ~> Tuple2_1Selector])

testExpressionIndex :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist Postgresql m ()
testExpressionIndex = do
  let val = ExpressionIndex 1
  migr val
  Just val @=?? (insert val >>= get)
  assertExc "expression index should fail on duplicates" $ insert $ ExpressionIndex (-1)
#endif

#if WITH_MYSQL
testSchemaAnalysisMySQL :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist MySQL m ()
testSchemaAnalysisMySQL = do
  let val = Single (Single "abc")
  migr val
  let action = "delete from `Single#String`;"
  executeRaw' ("CREATE TRIGGER `myTrigger` AFTER DELETE ON `Single#String` FOR EACH ROW BEGIN " ++ action ++ " END") []
  ["Single#Single#String", "Single#String"] @=?? liftM sort (listTables Nothing)
  ["myTrigger"] @=?? liftM sort (listTableTriggers (Nothing, "Single#String"))
  sql <- analyzeTrigger (Nothing, "myTrigger")
  let sql' = maybe (error "No trigger found") id sql
  liftIO $ action `isInfixOf` sql' H.@? "Trigger does not contain action statement"
  executeRaw' "CREATE FUNCTION myfunc() RETURNS decimal DETERMINISTIC BEGIN RETURN 42;END" []
  func <- analyzeFunction (Nothing, "myfunc")
  let (_, ret, body) = fromMaybe (error "No function found") func
  Just (DbOther (OtherTypeDef [Left "decimal(10,0)"])) @=? ret
  liftIO $ "RETURN 42" `isInfixOf` body H.@? "Function does not contain action statement"
#endif

assertExc :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => String -> m a -> m ()
assertExc err m = do
  happened <- control $ \runInIO -> E.catch (runInIO $ m >> return False) (\(_ :: SomeException) -> runInIO $ return True)
  unless happened $ liftIO (H.assertFailure err)

reescape :: DbDescriptor db => proxy db -> String -> String
reescape proxy query = if backendName proxy == "mysql"
  then map (\c -> if c == '"' then '`' else c) query
  else query

executeRaw' :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => String -> [PersistValue] -> m ()
executeRaw' query vals = do
  proxy <- phantomDb
  executeRaw True (reescape proxy query) vals

queryRaw' :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => String -> [PersistValue] -> (RowPopper m -> m a) -> m a
queryRaw' query vals f = do
  proxy <- phantomDb
  queryRaw True (reescape proxy query) vals f
