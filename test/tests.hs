{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, RankNTypes, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, StandaloneDeriving #-}
--module Main where
import qualified Control.Exception as E
import Control.Exception.Base (SomeException)
import Control.Monad (replicateM_, liftM, mapM_, forM_, (>=>), unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Migration (SchemaAnalyzer(..))
import Database.Groundhog.Generic.Sql
import Database.Groundhog.TH
import Database.Groundhog.Sqlite
import Database.Groundhog.Postgresql
import Database.Groundhog.Postgresql.Array hiding (all, any, append)
import qualified Database.Groundhog.Postgresql.Array as Arr
import Database.Groundhog.Postgresql.Geometry
import Database.Groundhog.MySQL
import Data.ByteString.Char8 (unpack)
import Data.Int
import Data.List (isInfixOf, sort)
import qualified Data.Map as Map
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
data Settable = Settable {settable1 :: String, settable2 :: Int, settableTuple :: (Int, (String, Double))} deriving (Eq, Show)
data Keys = Keys {refDirect :: Single String, refKey :: Key (Single String) BackendSpecific, refDirectMaybe :: Maybe (Single String), refKeyMaybe :: Maybe (Key (Single String) BackendSpecific)}
data EmbeddedSample = EmbeddedSample {embedded1 :: String, embedded2 :: (Int, Int)} deriving (Eq, Show)
data UniqueKeySample = UniqueKeySample { uniqueKey1 :: Int, uniqueKey2 :: Int, uniqueKey3 :: Int } deriving (Eq, Show)
data InCurrentSchema = InCurrentSchema { inCurrentSchema :: Maybe (Key InAnotherSchema BackendSpecific) }
data InAnotherSchema = InAnotherSchema { inAnotherSchema :: Maybe (Key InCurrentSchema BackendSpecific) }

-- cannot use ordinary deriving because it runs before mkPersist and requires (Single String) to be an instance of PersistEntity
deriving instance Eq Keys
deriving instance Show Keys
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
          type: primary
          fields: [uniqueKey1]
        - name: unique_key_two_columns
          fields: [uniqueKey2, uniqueKey3]
- entity: InCurrentSchema
- entity: InAnotherSchema
  schema: myschema
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

main :: IO ()
main = do
  let runSqlite m = withSqliteConn ":memory:" . runDbConn $ m
  let runPSQL m = withPostgresqlConn "dbname=test user=test password=test host=localhost" . runDbConn $ cleanPostgresql >> m
  let mySQLConnInfo = defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        }
  let runMySQL m = withMySQLConn mySQLConnInfo . runDbConn $ cleanMySQL >> m
  defaultMain [ testGroup "Database.Groundhog.MySQL" $ concatMap ($ runMySQL) [mkTestSuite, mkSqlTestSuite, mysqlTestSuite]
              , testGroup "Database.Groundhog.Sqlite" $ concatMap ($ runSqlite) [mkTestSuite, mkSqlTestSuite, sqliteTestSuite]
              , testGroup "Database.Groundhog.Postgresql" $ concatMap ($ runPSQL) [mkTestSuite, mkSqlTestSuite, postgresqlTestSuite]
              ]

migr :: (PersistEntity v, PersistBackend m, MonadBaseControl IO m, MonadIO m) => v -> m ()
migr v = do
  runMigration silentMigrationLogger (migrate v)
  m <- createMigration $ migrate v
  [] @=? filter (/= Right []) (Map.elems m)

mkSqlTestSuite :: (PersistBackend m, MonadBaseControl IO m, MonadIO m, db ~ PhantomDb m, SqlDb db, QueryRaw db ~ Snippet db) => (m () -> IO ()) -> [Test]
mkSqlTestSuite run =
  [ testCase "testSelect" $ run testSelect
  , testCase "testCond" $ run testCond
  , testCase "testProjectionSql" $ run testProjectionSql
  ]

mkTestSuite :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => (m () -> IO ()) -> [Test]
mkTestSuite run =
  [ testCase "testNumber" $ run testNumber
  , testCase "testPersistSettings" $ run testPersistSettings
  , testCase "testEmbedded" $ run testEmbedded
  , testCase "testInsert" $ run testInsert
  , testCase "testMaybe" $ run testMaybe
  , testCase "testCount" $ run testCount
  , testCase "testUpdate" $ run testUpdate
  , testCase "testComparison" $ run testComparison
  , testCase "testEncoding" $ run testEncoding
  , testCase "testDelete" $ run testDelete
  , testCase "testDeleteByKey" $ run testDeleteByKey
  , testCase "testReplaceSingle" $ run testReplaceSingle
  , testCase "testReplaceMulti" $ run testReplaceMulti
  , testCase "testTuple" $ run testTuple
  , testCase "testTupleList" $ run testTupleList
  , testCase "testMigrateAddColumnSingle" $ run testMigrateAddColumnSingle
  , testCase "testMigrateAddUniqueConstraint" $ run testMigrateAddUniqueConstraint
  , testCase "testMigrateDropUniqueConstraint" $ run testMigrateDropUniqueConstraint
  , testCase "testMigrateAddUniqueIndex" $ run testMigrateAddUniqueIndex
  , testCase "testMigrateDropUniqueIndex" $ run testMigrateDropUniqueIndex
  , testCase "testMigrateAddDropNotNull" $ run testMigrateAddDropNotNull
  , testCase "testMigrateAddConstructorToMany" $ run testMigrateAddConstructorToMany
  , testCase "testMigrateChangeType" $ run testMigrateChangeType
  , testCase "testLongNames" $ run testLongNames
  , testCase "testReference" $ run testReference
  , testCase "testMaybeReference" $ run testMaybeReference
  , testCase "testUniqueKey" $ run testUniqueKey
  , testCase "testForeignKeyUnique" $ run testForeignKeyUnique
  , testCase "testProjection" $ run testProjection
  , testCase "testKeyNormalization" $ run testKeyNormalization
  , testCase "testAutoKeyField" $ run testAutoKeyField
  , testCase "testTime" $ run testTime
  ]

sqliteTestSuite :: (MonadBaseControl IO m, MonadIO m) => (DbPersist Sqlite m () -> IO ()) -> [Test]
sqliteTestSuite run = 
  [ testCase "testMigrateOrphanConstructors" $ run testMigrateOrphanConstructors
  , testCase "testSchemaAnalysisSqlite" $ run testSchemaAnalysisSqlite
  , testCase "testListTriggersOnDelete" $ run testListTriggersOnDelete
  , testCase "testListTriggersOnUpdate" $ run testListTriggersOnUpdate
  ]


postgresqlTestSuite :: (MonadBaseControl IO m, MonadIO m) => (DbPersist Postgresql m () -> IO ()) -> [Test]
postgresqlTestSuite run =
  [ testCase "testGeometry" $ run testGeometry
  , testCase "testArrays" $ run testArrays
  , testCase "testSchemas" $ run testSchemas
  , testCase "testSchemaAnalysisPostgresql" $ run testSchemaAnalysisPostgresql
  , testCase "testListTriggersOnDelete" $ run testListTriggersOnDelete
  , testCase "testListTriggersOnUpdate" $ run testListTriggersOnUpdate
  ]


mysqlTestSuite :: (MonadBaseControl IO m, MonadIO m) => (DbPersist MySQL m () -> IO ()) -> [Test]
mysqlTestSuite run =
  [ testCase "testSchemas" $ run testSchemas
  , testCase "testSchemaAnalysisMySQL" $ run testSchemaAnalysisMySQL
--  , testCase "testListTriggersOnDelete" $ run testListTriggersOnDelete  -- fails due to MySQL bug #11472
--  , testCase "testListTriggersOnUpdate" $ run testListTriggersOnUpdate  -- fails due to MySQL bug #11472
  ]

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
  minNumber' <- insert minNumber >>= get
  maxNumber' <- insert maxNumber >>= get
  Just minNumber @=? minNumber'
  Just maxNumber @=? maxNumber'

testPersistSettings :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testPersistSettings = do
  let settable = Settable "abc" 1 (1, ("qqq", 2))
  m <- createMigration $ migrate settable
  let expectedNames = ["settable_id", "sqlsettable1", "firstTupleElement", "secondTupleElement", "thirdTupleElement", "someconstraint", "varchar(50)"]
  liftIO $ all (`isInfixOf` show m) expectedNames H.@? ("Should contain " ++ show expectedNames ++ ": " ++ show m)
  migr settable
  proxy <- phantomDb
  Just settable @=?? (insert settable >>= get)
  vals <- select $ Settable1Fld ==. "abc" &&. SettableTupleField ~> Tuple2_0Selector ==. (1 :: Int) &&. SettableTupleField ~> Tuple2_1Selector ~> Tuple2_0Selector ==. "qqq"
  [settable] @=? vals
  assertExc "Uniqueness constraint not enforced" $ insert settable

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
  k <- insert val
  val' <- get k
  Just val @=? val'

testSelect :: (PersistBackend m, MonadBaseControl IO m, MonadIO m, db ~ PhantomDb m, SqlDb db, QueryRaw db ~ Snippet db) => m ()
testSelect = do
  migr (undefined :: Single (Int, String))
  let val1 = Single (5 :: Int, "abc")
  let val2 = Single (7 :: Int, "DEF")
  let val3 = Single (11 :: Int, "ghc")
  k1 <- insert val1
  k2 <- insert val2
  k3 <- insert val3
  vals1 <- select $ (SingleField ~> Tuple2_0Selector >. (5 :: Int)) `orderBy` [Asc (SingleField ~> Tuple2_1Selector)] `offsetBy` 1
  [val3] @=? vals1
  vals2 <- select $ (SingleField ~> Tuple2_0Selector >. (5 :: Int)) `orderBy` [Asc (SingleField ~> Tuple2_1Selector)] `limitTo` 1
  [val2] @=? vals2
  vals3 <- select $ (SingleField >=. (6 :: Int, "something") &&. SingleField ~> Tuple2_1Selector <. "ghc") `limitTo` 1
  [val2] @=? vals3
  vals4 <- select $ toArith (SingleField ~> Tuple2_0Selector) + 1 >. (10 :: Int)
  [val3] @=? vals4
  vals5 <- select $ (SingleField ~> Tuple2_1Selector) `like` "%E%"
  [val2] @=? vals5
  vals6 <- select $ lower (SingleField ~> Tuple2_1Selector) ==. "def"
  [val2] @=? vals6
  vals7 <- select $ ((SingleField ~> Tuple2_0Selector) `in_` [7 :: Int, 5]) `orderBy` [Asc (SingleField ~> Tuple2_0Selector)]
  [val1, val2] @=? vals7

testCond :: forall m db . (PersistBackend m, MonadBaseControl IO m, MonadIO m, db ~ PhantomDb m, SqlDb db, QueryRaw db ~ Snippet db) => m ()
testCond = do
  proxy <- phantomDb
  let rend :: forall r . Cond (PhantomDb m) r -> Maybe (RenderS (PhantomDb m) r)
      rend = renderCond id (\a b -> a <> fromString "=" <> b) (\a b -> a <> fromString "<>" <> b)
  let (===) :: forall r a. (PrimitivePersistField a) => (String, [a]) -> Cond (PhantomDb m) r -> m ()
      (query, vals) === cond = let Just (RenderS q v) = rend cond in (query, map (toPrimitivePersistValue proxy) vals) @=? (unpack $ fromUtf8 $ q, v [])

  let intField f = f `asTypeOf` (undefined :: Field (Single (Int, Int)) c a)
      intNum = fromInteger :: Integer -> Expr db r Int
  -- should cover all cases of renderCond comparison rendering
  ("int=?", [4 :: Int]) === (IntField ==. (4 :: Int))
  ("int=int", [] :: [Int]) === (IntField ==. IntField)
  ("int=(int+?)*?", [1, 2 :: Int]) === (IntField ==. (toArith IntField + 1) * 2)

  ("single#val0=? AND single#val1=?", ["abc", "def"]) === (SingleField ==. ("abc", "def"))
  ("single#val0=single#val1", [] :: [Int]) === (intField SingleField ~> Tuple2_0Selector ==. SingleField ~> Tuple2_1Selector)
  ("single#val1=single#val0*(?+single#val0)", [5 :: Int]) === (intField SingleField ~> Tuple2_1Selector ==. toArith (SingleField ~> Tuple2_0Selector) * (5 + toArith (SingleField ~> Tuple2_0Selector)))

  ("?=? AND ?=?", [1, 2, 3, 4 :: Int]) === ((1 :: Int, 3 :: Int) ==. (2 :: Int, 4 :: Int) &&. SingleField ==. ()) -- SingleField ==. () is required to replace Any with a PersistEntity instance
  ("?<? OR ?<?", [1, 2, 3, 4 :: Int]) === ((1 :: Int, 3 :: Int) <. (2 :: Int, 4 :: Int) &&. SingleField ==. ())
  ("?=single#val0 AND ?=single#val1", [1, 2 :: Int]) === ((1 :: Int, 2 :: Int) ==. SingleField)
  ("?=single+?*?", [1, 2, 3 :: Int]) === ((1 :: Int) ==. toArith SingleField + 2 * 3)

--  ("?-single=?", [1, 2 :: Int]) === (1 - toArith SingleField ==. (2 :: Int))
  ("?*single>=single", [1 :: Int]) === (intNum 1 * toArith SingleField >=. SingleField)
--  ("?+single>=single-?", [1, 2 :: Int]) === (intNum 1 + toArith SingleField >=. toArith SingleField - 2)
  
  -- test parentheses
  ("NOT (NOT single=? OR ?=? AND ?=?)", [0, 1, 2, 3, 4 :: Int]) === (Not $ Not (SingleField ==. (0 :: Int)) ||. (1 :: Int, 3 :: Int) ==. (2 :: Int, 4 :: Int))
  ("single=? AND (?<? OR ?<?)", [0, 1, 2, 3, 4 :: Int]) === (SingleField ==. (0 :: Int) &&. (1 :: Int, 3 :: Int) <. (2 :: Int, 4 :: Int))
  ("NOT (single=? AND (single=single OR single<>single))", [0 :: Int]) === (Not $ SingleField ==. (0 :: Int) &&. (SingleField ==. SingleField ||. SingleField /=. SingleField))
  
  -- test empty conditions
  ("single#val0=? AND single#val1=?", ["abc", "def"]) === (SingleField ==. ("abc", "def") &&. (() ==. () ||. ((), ()) <. ((), ())))
  ("single#val0=? AND single#val1=?", ["abc", "def"]) === ((() ==. () ||. ((), ()) <. ((), ())) &&. SingleField ==. ("abc", "def"))
  

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
  --TODO: use key field when implemented
  --  update [SingleField ~> Tuple2_0Selector =. "abc", SingleField ~> Tuple2_1Selector =. "def"] (KeyIs k)
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
  deleteByKey k
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

testDeleteByKey :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testDeleteByKey = do
  migr (undefined :: Multi String)
  proxy <- phantomDb
  k <- insert $ Second "abc"
  deleteByKey k
  Nothing @=?? queryRaw' "SELECT * FROM \"Multi#String\" WHERE id=?" [toPrimitivePersistValue proxy k] firstRow
  Nothing @=?? queryRaw' "SELECT * FROM \"Multi#String#Second\" WHERE id=?" [toPrimitivePersistValue proxy k] firstRow

testReplaceMulti :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testReplaceMulti = do
  migr (undefined :: Single (Multi String))
  proxy <- phantomDb
  -- we need Single to test that referenced value cam be replaced
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
  -- we need Single to test that referenced value cam be replaced
  let val = Single (Single "abc")
  migr val
  proxy <- phantomDb
  k <- insert val
  Just [valueKey'] <- queryRaw' "SELECT \"single\" FROM \"Single#Single#String\" WHERE id=?" [toPrimitivePersistValue proxy k] firstRow
  let valueKey = fromPrimitivePersistValue proxy valueKey'
  
  replace valueKey (Single "def")
  replaced <- get valueKey
  Just (Single "def") @=? replaced

testMigrateAddColumnSingle :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMigrateAddColumnSingle = do
  migr (undefined :: Old.AddColumn)
  migr (undefined :: New.AddColumn)
  m <- createMigration $ migrate (undefined :: New.AddColumn)
  [] @=? filter (/= Right []) (Map.elems m)
  let val = New.AddColumn (Just "abc") 5
  k <- insert val
  val' <- get k
  Just val @=? val'

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
  insert val >>= deleteByKey
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
  k <- insert val
  val' <- get k
  Just val @=? val'

  let val2 = Single [([""], Single "", 0 :: Int)]
  migr val2
  m2 <- createMigration (migrate val2)
  executeMigration silentMigrationLogger m2
  -- this might fail because the constraint names are too long. They constraints are created successfully, but with stripped names. Then during the second migration the stripped names differ from expected and this leads to migration errors.
  [] @=? filter (/= Right []) (Map.elems m2)

testReference :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testReference = do
  migr (undefined :: Single (Key (Single String) BackendSpecific))
  k <- insert $ Single "abc"
  insert $ Single k
  assertExc "Foreign key must prevent deletion" $ deleteByKey k

testMaybeReference :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testMaybeReference = do
  migr (undefined :: Single (Maybe (Key (Single String) BackendSpecific)))
  k <- insert $ Single "abc"
  insert $ Single $ Just k
  assertExc "Foreign key must prevent deletion" $ deleteByKey k

testUniqueKey :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testUniqueKey = do
  let uVal = UniqueKeySample 1 2 3
  let val = Single uVal
  migr val
  k <- insert val
  val1 <- get k
  Just val @=? val1
  val2 <- select (Unique_key_two_columns ==. Unique_key_two_columnsKey 2 3)
  [uVal] @=? val2

testForeignKeyUnique :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testForeignKeyUnique = do
  let uVal = UniqueKeySample 1 2 3
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
  result <- project (AutoKeyField, SingleConstructor, SingleField, SingleField ~> Tuple2_1Selector) ("" ==. "")
  [(k, val, ("abc", 5 :: Int), 5 :: Int)] @=? result
  let uVal = UniqueKeySample 1 2 3
  migr uVal
  insert uVal
  result2 <- project Unique_key_two_columns ("" ==. "")
  [extractUnique uVal] @=? result2

testProjectionSql :: (PersistBackend m, MonadBaseControl IO m, MonadIO m, db ~ PhantomDb m, SqlDb db, QueryRaw db ~ Snippet db) => m ()
testProjectionSql = do
  let val = Single ("abc", 5 :: Int)
  migr val
  k <- insert val
  result <- project ("hello " `append` (upper $ SingleField ~> Tuple2_0Selector), toArith (SingleField ~> Tuple2_1Selector) + 1) (() ==. ())
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
  result <- select $ AutoKeyField `asTypeOf` (undefined :: f v SingleConstructor) ==. k
  [val] @=? result

-- This test must just compile
testKeys :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testKeys = do
  migr (undefined :: Keys)
  k <- insert $ Single ""
  let cond = RefDirectField ==. k ||. RefKeyField ==. k ||. RefDirectMaybeField ==. Just k ||. RefKeyMaybeField ==. Just k
  select cond
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
  val' <- get k
  Just val @=? val'

testGeometry :: (MonadBaseControl IO m, MonadIO m) => DbPersist Postgresql m ()
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

testArrays :: (MonadBaseControl IO m, MonadIO m) => DbPersist Postgresql m ()
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
  [Array [1..4]] @=?? project (SingleField `Arr.append` int 4) (AutoKeyField ==. k)
  [Array [0..3]] @=?? project (int 0 `Arr.prepend` SingleField) (AutoKeyField ==. k)
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
  [k] @=?? project AutoKeyField (AutoKeyField ==. k &&. SingleField @> Array [int 2, 1])
  [k] @=?? project AutoKeyField (AutoKeyField ==. k &&. Array [int 2, 1] <@ SingleField)
  [k] @=?? project AutoKeyField (AutoKeyField ==. k &&. Array [int 3..10] `overlaps` SingleField)
  -- test escaping/unescaping
  let myString = ['\1'..'\255'] :: String
  let val2 = Single (Array[myString], Array[Array[myString]])
  migr val2
  Just val2 @=?? (insert val2 >>= get)

testSchemas :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => m ()
testSchemas = do
  let val = InCurrentSchema Nothing
  migr val -- InAnotherSchema will be migrated automatically
  k <- insert val
  let val2 = InAnotherSchema (Just k)
  Just val2 @=?? (insert val2 >>= get)

testSchemaAnalysisSqlite :: (MonadBaseControl IO m, MonadIO m) => DbPersist Sqlite m ()
testSchemaAnalysisSqlite = do
  let val = Single (Single "abc")
  migr val
  let action = "select * from \"Single#String\";"
  executeRaw False ("CREATE TRIGGER \"myTrigger\" AFTER DELETE ON \"Single#String\" FOR EACH ROW BEGIN " ++ action ++ " END") []
  ["Single#Single#String", "Single#String"] @=?? liftM sort (listTables Nothing)
  ["myTrigger"] @=?? liftM sort (listTableTriggers Nothing "Single#String")
  sql <- analyzeTrigger Nothing "myTrigger"
  let sql' = maybe (error "No trigger found") id sql
  liftIO $ action `isInfixOf` sql' H.@? "Trigger does not contain action statement"

testSchemaAnalysisPostgresql :: (MonadBaseControl IO m, MonadIO m) => DbPersist Postgresql m ()
testSchemaAnalysisPostgresql = do
  let val = Single (Single "abc")
  migr val
  let action = "EXECUTE PROCEDURE \"myFunction\"()"
  executeRaw False "CREATE OR REPLACE FUNCTION \"myFunction\"() RETURNS trigger AS $$ BEGIN RETURN NEW;END; $$ LANGUAGE plpgsql" []
  executeRaw False ("CREATE TRIGGER \"myTrigger\" AFTER DELETE ON \"Single#String\" FOR EACH ROW " ++ action) []
  ["Single#Single#String", "Single#String"] @=?? liftM sort (listTables Nothing)
  ["myTrigger"] @=?? liftM sort (listTableTriggers Nothing "Single#String")
  trigSql <- analyzeTrigger Nothing "myTrigger"
  let trigSql' = maybe (error "No trigger found") id trigSql
  liftIO $ action `isInfixOf` trigSql' H.@? "Trigger does not contain action statement"
  funcSql <- analyzeFunction Nothing "myFunction"
  let funcSql' = maybe (error "No function found") id funcSql
  liftIO $ "RETURN NEW;" `isInfixOf` funcSql' H.@? "Function does not contain action statement"

testSchemaAnalysisMySQL :: (MonadBaseControl IO m, MonadIO m) => DbPersist MySQL m ()
testSchemaAnalysisMySQL = do
  let val = Single (Single "abc")
  migr val
  let action = "delete from `Single#String`;"
  executeRaw' ("CREATE TRIGGER `myTrigger` AFTER DELETE ON `Single#String` FOR EACH ROW BEGIN " ++ action ++ " END") []
  ["Single#Single#String", "Single#String"] @=?? liftM sort (listTables Nothing)
  ["myTrigger"] @=?? liftM sort (listTableTriggers Nothing "Single#String")
  sql <- analyzeTrigger Nothing "myTrigger"
  let sql' = maybe (error "No trigger found") id sql
  liftIO $ action `isInfixOf` sql' H.@? "Trigger does not contain action statement"
  executeRaw' "CREATE FUNCTION myfunc() RETURNS decimal DETERMINISTIC BEGIN RETURN 42;END" []
  funcSql <- analyzeFunction Nothing "myfunc"
  let funcSql' = maybe (error "No function found") id funcSql
  liftIO $ "RETURN 42" `isInfixOf` funcSql' H.@? "Function does not contain action statement"

-- TODO: write test which inserts data before adding new columns

firstRow :: Monad m => RowPopper m -> m (Maybe [PersistValue])
firstRow = id
  
createTruncateTables :: String
createTruncateTables = "CREATE OR REPLACE FUNCTION truncate_tables(username IN VARCHAR) RETURNS void AS $$\
\DECLARE\
\    statements CURSOR FOR SELECT tablename FROM pg_tables WHERE tableowner = username AND schemaname = 'public';\
\BEGIN\
\    FOR stmt IN statements LOOP\
\        EXECUTE 'TRUNCATE TABLE ' || quote_ident(stmt.tablename) || ' CASCADE;';\
\    END LOOP;\
\END;\
\$$ LANGUAGE plpgsql"

cleanPostgresql :: (MonadBaseControl IO m, MonadIO m) => DbPersist Postgresql m ()
cleanPostgresql = forM_ ["public", "myschema"] $ \schema -> do
  executeRaw True ("drop schema if exists " ++ schema ++ " cascade") []
  executeRaw True ("create schema " ++ schema) []
  executeRaw True ("alter schema " ++ schema ++ " owner to test") []

cleanMySQL :: (MonadBaseControl IO m, MonadIO m) => DbPersist MySQL m ()
cleanMySQL = do
  executeRaw True "SET FOREIGN_KEY_CHECKS = 0" []
  forM_ ["test", "myschema"] $ \schema -> do
    executeRaw True ("drop database if exists " ++ schema) []
    executeRaw True ("create database " ++ schema) []
  executeRaw True ("use test") []
  executeRaw True "SET FOREIGN_KEY_CHECKS = 1" []

assertExc :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => String -> m a -> m ()
assertExc err m = do
  happened <- control $ \runInIO -> E.catch (runInIO $ m >> return False) (\(e :: SomeException) -> runInIO $ return True)
  unless happened $ liftIO (H.assertFailure err)

reescape :: DbDescriptor db => Proxy db -> String -> String
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
