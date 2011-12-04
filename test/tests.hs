{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, RankNTypes #-}

import qualified Data.Map as M
import Control.Monad(replicateM_, liftM, forM_, (>=>))
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.IO.Control (MonadControlIO)
import Database.Groundhog.Core
import Database.Groundhog.TH
import Database.Groundhog.Sqlite
import Database.Groundhog.Postgresql
import Data.Int
import Data.Word
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

data User a = U1 {foo :: Int} | U2 { bar :: Maybe String, asc :: Int64, add :: a} | U3 deriving (Eq, Show)
data Person = Person {name :: String, age :: Int, height :: Int} deriving (Eq, Show)
data Number = Number {int :: Int, int8 :: Int8, word8 :: Word8, int16 :: Int16, word16 :: Word16, int32 :: Int32, word32 :: Word32, int64 :: Int64, word64 :: Word64} deriving (Eq, Show)
data TestTriggers = TestTriggers {testList :: [(Int, Int)], testTuple :: Maybe ([String], Int)} deriving (Eq, Show)

deriveEntity ''User Nothing
deriveEntity ''Person Nothing
deriveEntity ''Number Nothing
deriveEntity ''TestTriggers Nothing

main :: IO ()
main = do
  let runSqlite m = withSqliteConn ":memory:" . runSqliteConn $ setup >> m
  let runPSQL m = withPostgresqlConn "dbname=test user=test password=test host=localhost" . runPostgresqlConn $ m
  runPSQL setup
  -- we need clean db before each migration test
  defaultMain [ sqliteMigrationTestSuite $ withSqliteConn ":memory:" . runSqliteConn
              , mkTestSuite "Database.Groundhog.Sqlite" $ runSqlite
              , mkTestSuite "Database.Groundhog.Postgresql" $ runPSQL
              ]

setup :: (PersistBackend m, MonadIO m) => m ()
setup = do
  m <- createMigration $ do
    migrate (undefined :: User (String, Maybe (User String)))
    migrate (undefined :: User [User String])
    migrate (undefined :: User (Maybe String))
    migrate (undefined :: User String)
    migrate (undefined :: Person)
    migrate (undefined :: Number)
    migrate (undefined :: TestTriggers)
--  printMigration m
  executeMigration silentMigrationLogger m

mkTestSuite :: (PersistBackend m, MonadIO m) => String -> (m () -> IO ()) -> Test
mkTestSuite label run = testGroup label $
  [ testCase "testNumber" $ run testNumber
  , testCase "testInsert" $ run testInsert
  , testCase "testCount" $ run testCount
  , testCase "testEncoding" $ run testEncoding
  , testCase "testListTriggersOnDelete" $ run testListTriggersOnDelete
  , testCase "testTupleTriggersOnDelete" $ run testTupleTriggersOnDelete
  , testCase "testListTriggersOnUpdate" $ run testListTriggersOnUpdate
  , testCase "testTupleTriggersOnUpdate" $ run testTupleTriggersOnUpdate
  ]
  
sqliteMigrationTestSuite :: (PersistBackend m, MonadIO m) => (m () -> IO ()) -> Test
sqliteMigrationTestSuite run = testGroup "Database.Groundhog.Sqlite.Migration" $
  [ testCase "testOrphanConstructors" $ run testOrphanConstructors
  ]

(@=?) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
expected @=? actual = liftIO $ expected H.@=? actual

testOrphanConstructors :: (PersistBackend m, MonadIO m) => m ()
testOrphanConstructors = do
  setup
  executeRaw False "drop table User$String" []
  mig <- createMigration (migrate (undefined :: User String))
  [("User$String",Left ["Orphan constructor table found: User$String$U1","Orphan constructor table found: User$String$U2","Orphan constructor table found: User$String$U3"])] @=? M.toList mig

testNumber :: (PersistBackend m, MonadIO m) => m ()
testNumber = do
  let minNumber = Number minBound minBound minBound minBound minBound minBound minBound minBound minBound
  let maxNumber = Number maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound
  minNumber' <- insert minNumber >>= get
  maxNumber' <- insert maxNumber >>= get
  minNumber' @=? Just minNumber
  maxNumber' @=? Just maxNumber

testInsert :: (PersistBackend m, MonadIO m) => m ()
testInsert = do
  let jack = Person "Jack" 20 175
  k <- insert jack
  jack' <- get k
  jack' @=? Just jack

testCount :: (PersistBackend m, MonadIO m) => m ()
testCount = do
  insert $ (U1 0 :: User String)
  insert $ U2 Nothing 0 "abc"
  insert $ U2 (Just "abc") 0 "def"
  insert $ (U3 :: User String)
  num <- countAll (undefined :: User String)
  4 @=? num
  num2 <- count $ BarField /=. (Nothing :: Maybe String) ||. AddField /=. "def"
  2 @=? num2

testEncoding :: (PersistBackend m, MonadIO m) => m ()
testEncoding = do
  let person = Person "\x0001\x0081\x0801\x10001" 0 0
  k <- insert person
  Just person' <- get k
  person @=? person'

testListTriggersOnDelete :: (PersistBackend m, MonadIO m) => m ()
testListTriggersOnDelete = do
  k <- insert $ TestTriggers [(0, 0), (0, 0)] (Just (["abc"], 0))
  Just [listKey] <- queryRaw False "select \"testList\" from \"TestTriggers\" where id$=?" [toPrim k] firstRow
  tupleInsideListKeys <- queryRaw False "select value from \"List$$Tuple2$$Int$Int$values\" where id$=?" [listKey] $ mapAllRows return
  deleteByKey k
  -- test if the main list table and the associated values were deleted
  listMain <- queryRaw False "select * from \"List$$Tuple2$$Int$Int\" where id$=?" [listKey] firstRow
  Nothing @=? listMain
  listValues <- queryRaw False "select * from \"List$$Tuple2$$Int$Int$values\" where id$=?" [listKey] firstRow
  Nothing @=? listValues
  -- test if the ephemeral values associated with the list were deleted
  forM_ tupleInsideListKeys $ \tupleInsideListKey -> do
    tupleValues <- queryRaw False "select * from \"Tuple2$$Int$Int\" where id$=?" tupleInsideListKey firstRow
    Nothing @=? tupleValues

testTupleTriggersOnDelete :: (PersistBackend m, MonadIO m) => m ()
testTupleTriggersOnDelete = do
  k <- insert $ TestTriggers [(0, 0), (0, 0)] (Just (["abc"], 0))
  Just [tupleKey] <- queryRaw False "select \"testTuple\" from \"TestTriggers\" where id$=?" [toPrim k] firstRow
  Just [listInsideTupleKey] <- queryRaw False "select val0 from \"Tuple2$$List$$String$Int\" where id$=?" [tupleKey] firstRow
  deleteByKey k
  -- test if the tuple was deleted
  tupleValues <- queryRaw False "select val0 from \"Tuple2$$List$$String$Int\" where id$=?" [tupleKey] firstRow
  Nothing @=? tupleValues
  -- test if the ephemeral values associated with the tuple were deleted
  listMain <- queryRaw False "select * from \"List$$String\" where id$=?" [listInsideTupleKey] firstRow
  Nothing @=? listMain
  listValues <- queryRaw False "select * from \"List$$String$values\" where id$=?" [listInsideTupleKey] firstRow
  Nothing @=? listValues

testListTriggersOnUpdate :: (PersistBackend m, MonadIO m) => m ()
testListTriggersOnUpdate = do
  k <- insert $ TestTriggers [(0, 0), (0, 0)] (Just (["abc"], 0))
  Just [listKey] <- queryRaw False "select \"testList\" from \"TestTriggers\" where id$=?" [toPrim k] firstRow
  tupleInsideListKeys <- queryRaw False "select value from \"List$$Tuple2$$Int$Int$values\" where id$=?" [listKey] $ mapAllRows return
  replace k $ TestTriggers [] Nothing
  -- test if the old main list table and the associated values were deleted
  listMain <- queryRaw False "select * from \"List$$Tuple2$$Int$Int\" where id$=?" [listKey] firstRow
  Nothing @=? listMain
  listValues <- queryRaw False "select * from \"List$$Tuple2$$Int$Int$values\" where id$=?" [listKey] firstRow
  Nothing @=? listValues
  -- test if the ephemeral values associated with the old list were deleted
  forM_ tupleInsideListKeys $ \tupleInsideListKey -> do
    tupleValues <- queryRaw False "select * from \"Tuple2$$Int$Int\" where id$=?" tupleInsideListKey firstRow
    Nothing @=? tupleValues

testTupleTriggersOnUpdate :: (PersistBackend m, MonadIO m) => m ()
testTupleTriggersOnUpdate = do
  k <- insert $ TestTriggers [(0, 0), (0, 0)] (Just (["abc"], 0))
  Just [tupleKey] <- queryRaw False "select \"testTuple\" from \"TestTriggers\" where id$=?" [toPrim k] firstRow
  Just [listInsideTupleKey] <- queryRaw False "select val0 from \"Tuple2$$List$$String$Int\" where id$=?" [tupleKey] firstRow
  replace k $ TestTriggers [] Nothing
  -- test if the old tuple was deleted
  tupleValues <- queryRaw False "select val0 from \"Tuple2$$List$$String$Int\" where id$=?" [tupleKey] firstRow
  Nothing @=? tupleValues
  -- test if the ephemeral values associated with the old tuple were deleted
  listMain <- queryRaw False "select * from \"List$$String\" where id$=?" [listInsideTupleKey] firstRow
  Nothing @=? listMain
  listValues <- queryRaw False "select * from \"List$$String$values\" where id$=?" [listInsideTupleKey] firstRow
  Nothing @=? listValues

firstRow pop = pop >>= return

mapAllRows :: Monad m => ([PersistValue] -> m a) -> RowPopper m -> m [a]
mapAllRows f pop = go where
  go = pop >>= maybe (return []) (f >=> \a -> liftM (a:) go)
