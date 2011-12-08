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

data Number = Number {int :: Int, int8 :: Int8, word8 :: Word8, int16 :: Int16, word16 :: Word16, int32 :: Int32, word32 :: Word32, int64 :: Int64, word64 :: Word64} deriving (Eq, Show)
data Single a = Single {single :: a} deriving (Eq, Show)
data Multi a = First {first :: Int} | Second {second :: a} deriving (Eq, Show)

--deriveEntity ''User Nothing
--deriveEntity ''Person Nothing
deriveEntity ''Number Nothing
deriveEntity ''Single Nothing
deriveEntity ''Multi Nothing

main :: IO ()
main = do
  let runSqlite m = withSqliteConn ":memory:" . runSqliteConn $ m
  let runPSQL m = withPostgresqlConn "dbname=test user=test password=test host=localhost" . runPostgresqlConn $ clean >> m
--  runPSQL $ setup >> executeRaw False create_truncate_tables []
  -- we need clean db before each migration test
  defaultMain [ sqliteMigrationTestSuite $ withSqliteConn ":memory:" . runSqliteConn
              , mkTestSuite "Database.Groundhog.Sqlite" $ runSqlite
              , mkTestSuite "Database.Groundhog.Postgresql" $ runPSQL
              ]

migr :: (PersistBackend m, MonadIO m) => Migration m -> m ()
migr = runMigration silentMigrationLogger

mkTestSuite :: (PersistBackend m, MonadIO m) => String -> (m () -> IO ()) -> Test
mkTestSuite label run = testGroup label $
  [ testCase "testNumber" $ run testNumber
  , testCase "testInsert" $ run testInsert
  , testCase "testCount" $ run testCount
  , testCase "testEncoding" $ run testEncoding
  , testCase "testDelete" $ run testDelete
  , testCase "testDeleteByKey" $ run testDeleteByKey
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
  migr $ migrate (undefined :: Multi String)
  executeRaw False "drop table Multi$String" []
  mig <- createMigration (migrate (undefined :: Multi String))
  [("Multi$String", Left ["Orphan constructor table found: Multi$String$First","Orphan constructor table found: Multi$String$Second"])] @=? M.toList mig

testNumber :: (PersistBackend m, MonadIO m) => m ()
testNumber = do
  migr $ migrate (undefined :: Number)
  let minNumber = Number minBound minBound minBound minBound minBound minBound minBound minBound minBound
  let maxNumber = Number maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound
  minNumber' <- insert minNumber >>= get
  maxNumber' <- insert maxNumber >>= get
  minNumber' @=? Just minNumber
  maxNumber' @=? Just maxNumber

testInsert :: (PersistBackend m, MonadIO m) => m ()
testInsert = do
  migr $ migrate (undefined :: Single String)
  let val = Single "abc"
  k <- insert val
  val' <- get k
  val' @=? Just val

testCount :: (PersistBackend m, MonadIO m) => m ()
testCount = do
  migr $ migrate (undefined :: Multi String)
  insert $ (First 0 :: Multi String)
  insert $ (Second "abc")
  num <- countAll (undefined :: Multi String)
  2 @=? num
  num2 <- count $ SecondField ==. "abc"
  1 @=? num2

testEncoding :: (PersistBackend m, MonadIO m) => m ()
testEncoding = do
  migr $ migrate (undefined :: Single String)
  let val = Single "\x0001\x0081\x0801\x10001"
  k <- insert val
  Just val' <- get k
  val @=? val'

testListTriggersOnDelete :: (PersistBackend m, MonadIO m) => m ()
testListTriggersOnDelete = do
  migr $ migrate (undefined :: Single [(Int, Int)])
  k <- insert $ (Single [(0, 0), (0, 0)] :: Single [(Int, Int)])
  Just [listKey] <- queryRaw False "select \"single\" from \"Single$List$$Tuple2$$Int$Int\" where id$=?" [toPrim k] firstRow
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
  migr $ migrate (undefined :: Single (Maybe ([String], Int)))
  k <- insert $ (Single (Just (["abc"], 0)) :: Single (Maybe ([String], Int)))
  Just [tupleKey] <- queryRaw False "select \"single\" from \"Single$Maybe$Tuple2$$List$$String$Int\" where id$=?" [toPrim k] firstRow
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
  migr $ migrate (undefined :: Single [(Int, Int)])
  k <- insert $ (Single [(0, 0), (0, 0)] :: Single [(Int, Int)])
  Just [listKey] <- queryRaw False "select \"single\" from \"Single$List$$Tuple2$$Int$Int\" where id$=?" [toPrim k] firstRow
  tupleInsideListKeys <- queryRaw False "select value from \"List$$Tuple2$$Int$Int$values\" where id$=?" [listKey] $ mapAllRows return
  replace k $ (Single [] :: Single [(Int, Int)])
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
  migr $ migrate (undefined :: Single (Maybe ([String], Int)))
  k <- insert $ (Single (Just (["abc"], 0)) :: Single (Maybe ([String], Int)))
  Just [tupleKey] <- queryRaw False "select \"single\" from \"Single$Maybe$Tuple2$$List$$String$Int\" where id$=?" [toPrim k] firstRow
  Just [listInsideTupleKey] <- queryRaw False "select val0 from \"Tuple2$$List$$String$Int\" where id$=?" [tupleKey] firstRow
  replace k $ (Single Nothing :: Single (Maybe ([String], Int)))
  -- test if the old tuple was deleted
  tupleValues <- queryRaw False "select val0 from \"Tuple2$$List$$String$Int\" where id$=?" [tupleKey] firstRow
  Nothing @=? tupleValues
  -- test if the ephemeral values associated with the old tuple were deleted
  listMain <- queryRaw False "select * from \"List$$String\" where id$=?" [listInsideTupleKey] firstRow
  Nothing @=? listMain
  listValues <- queryRaw False "select * from \"List$$String$values\" where id$=?" [listInsideTupleKey] firstRow
  Nothing @=? listValues

testDelete :: (PersistBackend m, MonadIO m) => m ()
testDelete = do
  migr $ migrate (undefined :: Multi String)
  k <- insert $ Second "abc"
  delete $ SecondField ==. "abc"
  main <- queryRaw True "SELECT * FROM \"Multi$String\" WHERE id$=?" [toPrim k] firstRow
  Nothing @=? main
  constr <- queryRaw True "SELECT * FROM \"Multi$String$Second\" WHERE id$=?" [toPrim k] firstRow
  Nothing @=? constr

testDeleteByKey :: (PersistBackend m, MonadIO m) => m ()
testDeleteByKey = do
  migr $ migrate (undefined :: Multi String)
  k <- insert $ Second "abc"
  deleteByKey k
  main <- queryRaw True "SELECT * FROM \"Multi$String\" WHERE id$=?" [toPrim k] firstRow
  Nothing @=? main
  constr <- queryRaw True "SELECT * FROM \"Multi$String$Second\" WHERE id$=?" [toPrim k] firstRow
  Nothing @=? constr

firstRow pop = pop >>= return

mapAllRows :: Monad m => ([PersistValue] -> m a) -> RowPopper m -> m [a]
mapAllRows f pop = go where
  go = pop >>= maybe (return []) (f >=> \a -> liftM (a:) go)
  
create_truncate_tables :: String
create_truncate_tables = "CREATE OR REPLACE FUNCTION truncate_tables(username IN VARCHAR) RETURNS void AS $$\
\DECLARE\
\    statements CURSOR FOR SELECT tablename FROM pg_tables WHERE tableowner = username AND schemaname = 'public';\
\BEGIN\
\    FOR stmt IN statements LOOP\
\        EXECUTE 'TRUNCATE TABLE ' || quote_ident(stmt.tablename) || ' CASCADE;';\
\    END LOOP;\
\END;\
\$$ LANGUAGE plpgsql"

clean :: (PersistBackend m, MonadIO m) => m ()
clean = do
  executeRaw True "drop schema public cascade" []
  executeRaw True "create schema public" []
