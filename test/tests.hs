{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, RankNTypes #-}

import qualified Data.Map as M
import Control.Exception.Base(SomeException)
import Control.Exception.Control (catch)
import Control.Monad(replicateM_, liftM, forM_, (>=>), unless)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.IO.Control (MonadControlIO)
import Database.Groundhog.Core
import Database.Groundhog.TH
import Database.Groundhog.Sqlite
import Database.Groundhog.Postgresql
import Data.Int
import qualified Data.Map as Map
import Data.Word
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Migration.Old as Old
import qualified Migration.New as New
import qualified Test.HUnit as H
import Prelude hiding (catch)

data Number = Number {int :: Int, int8 :: Int8, word8 :: Word8, int16 :: Int16, word16 :: Word16, int32 :: Int32, word32 :: Word32, int64 :: Int64, word64 :: Word64} deriving (Eq, Show)
data Single a = Single {single :: a} deriving (Eq, Show)
data Multi a = First {first :: Int} | Second {second :: a} deriving (Eq, Show)

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

migr :: (PersistEntity v, PersistBackend m, MonadControlIO m) => v -> m ()
migr v = runMigration silentMigrationLogger (migrate v)

mkTestSuite :: (PersistBackend m, MonadControlIO m) => String -> (m () -> IO ()) -> Test
mkTestSuite label run = testGroup label $
  [ testCase "testNumber" $ run testNumber
  , testCase "testInsert" $ run testInsert
  , testCase "testCount" $ run testCount
  , testCase "testEncoding" $ run testEncoding
  , testCase "testDelete" $ run testDelete
  , testCase "testDeleteByKey" $ run testDeleteByKey
  , testCase "testReplaceSingle" $ run testReplaceSingle
  , testCase "testReplaceMulti" $ run testReplaceMulti
  , testCase "testListTriggersOnDelete" $ run testListTriggersOnDelete
  , testCase "testTupleTriggersOnDelete" $ run testTupleTriggersOnDelete
  , testCase "testListTriggersOnUpdate" $ run testListTriggersOnUpdate
  , testCase "testTupleTriggersOnUpdate" $ run testTupleTriggersOnUpdate
  , testCase "testMigrateAddColumnSingle" $ run testMigrateAddColumnSingle
  , testCase "testMigrateAddConstructorToMany" $ run testMigrateAddConstructorToMany
  , testCase "testReference" $ run testReference
  , testCase "testMaybeReference" $ run testMaybeReference
  ]
  
sqliteMigrationTestSuite :: (PersistBackend m, MonadControlIO m) => (m () -> IO ()) -> Test
sqliteMigrationTestSuite run = testGroup "Database.Groundhog.Sqlite.Migration" $
  [ testCase "testOrphanConstructors" $ run testOrphanConstructors
  ]

(@=?) :: (Eq a, Show a, MonadControlIO m) => a -> a -> m ()
expected @=? actual = liftIO $ expected H.@=? actual

testOrphanConstructors :: (PersistBackend m, MonadControlIO m) => m ()
testOrphanConstructors = do
  migr (undefined :: Multi String)
  executeRaw False "drop table Multi$String" []
  mig <- createMigration (migrate (undefined :: Multi String))
  [("Multi$String", Left ["Orphan constructor table found: Multi$String$First","Orphan constructor table found: Multi$String$Second"])] @=? M.toList mig

testNumber :: (PersistBackend m, MonadControlIO m) => m ()
testNumber = do
  migr (undefined :: Number)
  let minNumber = Number minBound minBound minBound minBound minBound minBound minBound minBound minBound
  let maxNumber = Number maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound
  minNumber' <- insert minNumber >>= get
  maxNumber' <- insert maxNumber >>= get
  minNumber' @=? Just minNumber
  maxNumber' @=? Just maxNumber

testInsert :: (PersistBackend m, MonadControlIO m) => m ()
testInsert = do
  migr (undefined :: Single String)
  let val = Single "abc"
  k <- insert val
  val' <- get k
  val' @=? Just val

testCount :: (PersistBackend m, MonadControlIO m) => m ()
testCount = do
  migr (undefined :: Multi String)
  insert $ (First 0 :: Multi String)
  insert $ (Second "abc")
  num <- countAll (undefined :: Multi String)
  2 @=? num
  num2 <- count $ SecondField ==. "abc"
  1 @=? num2

testEncoding :: (PersistBackend m, MonadControlIO m) => m ()
testEncoding = do
  migr (undefined :: Single String)
  let val = Single "\x0001\x0081\x0801\x10001"
  k <- insert val
  Just val' <- get k
  val @=? val'

testListTriggersOnDelete :: (PersistBackend m, MonadControlIO m) => m ()
testListTriggersOnDelete = do
  migr (undefined :: Single [(Int, Int)])
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

testTupleTriggersOnDelete :: (PersistBackend m, MonadControlIO m) => m ()
testTupleTriggersOnDelete = do
  migr (undefined :: Single (Maybe ([String], Int)))
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

testListTriggersOnUpdate :: (PersistBackend m, MonadControlIO m) => m ()
testListTriggersOnUpdate = do
  migr (undefined :: Single [(Int, Int)])
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

testTupleTriggersOnUpdate :: (PersistBackend m, MonadControlIO m) => m ()
testTupleTriggersOnUpdate = do
  migr (undefined :: Single (Maybe ([String], Int)))
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

testDelete :: (PersistBackend m, MonadControlIO m) => m ()
testDelete = do
  migr (undefined :: Multi String)
  k <- insert $ Second "abc"
  delete $ SecondField ==. "abc"
  main <- queryRaw True "SELECT * FROM \"Multi$String\" WHERE id$=?" [toPrim k] firstRow
  Nothing @=? main
  constr <- queryRaw True "SELECT * FROM \"Multi$String$Second\" WHERE id$=?" [toPrim k] firstRow
  Nothing @=? constr

testDeleteByKey :: (PersistBackend m, MonadControlIO m) => m ()
testDeleteByKey = do
  migr (undefined :: Multi String)
  k <- insert $ Second "abc"
  deleteByKey k
  main <- queryRaw True "SELECT * FROM \"Multi$String\" WHERE id$=?" [toPrim k] firstRow
  Nothing @=? main
  constr <- queryRaw True "SELECT * FROM \"Multi$String$Second\" WHERE id$=?" [toPrim k] firstRow
  Nothing @=? constr

testReplaceMulti :: (PersistBackend m, MonadControlIO m) => m ()
testReplaceMulti = do
  migr (undefined :: Single (Multi String))
  -- we need Single to test that referenced value cam be replaced
  k <- insert $ Single (Second "abc")
  Just [valueKey'] <- queryRaw True "SELECT \"single\" FROM \"Single$Multi$String\" WHERE id$=?" [toPrim k] firstRow
  let valueKey = fromPrim valueKey'

  replace valueKey (Second "def")
  replaced <- get valueKey
  Just (Second "def") @=? replaced

  replace valueKey (First 5)
  replaced <- get valueKey
  Just (First 5) @=? replaced
  oldConstructor <- queryRaw True "SELECT * FROM \"Multi$String$Second\" WHERE id$=?" [toPrim valueKey] firstRow
  Nothing @=? oldConstructor

testReplaceSingle :: (PersistBackend m, MonadControlIO m) => m ()
testReplaceSingle = do
  migr (undefined :: Single (Single String))
  -- we need Single to test that referenced value cam be replaced
  k <- insert $ Single (Single "abc")
  Just [valueKey'] <- queryRaw True "SELECT \"single\" FROM \"Single$Single$String\" WHERE id$=?" [toPrim k] firstRow
  let valueKey = fromPrim valueKey'
  
  replace valueKey (Single "def")
  replaced <- get valueKey
  Just (Single "def") @=? replaced

testMigrateAddColumnSingle :: (PersistBackend m, MonadControlIO m) => m ()
testMigrateAddColumnSingle = do
  migr (undefined :: Old.AddColumn)
  migr (undefined :: New.AddColumn)
  m <- createMigration $ migrate (undefined :: New.AddColumn)
  Map.singleton "AddColumn" (Right []) @=? m
  let val = New.AddColumn "abc" 5
  k <- insert val
  val' <- get k
  Just val @=? val'

testMigrateAddConstructorToMany :: (PersistBackend m, MonadControlIO m) => m ()
testMigrateAddConstructorToMany = do
  migr (undefined :: Old.AddConstructorToMany)
  Key k1 <- insert $ Old.AddConstructorToMany1 1
  Key k2 <- insert $ Old.AddConstructorToMany2 "abc"
  migr (undefined :: New.AddConstructorToMany)
  k0 <- insert $ New.AddConstructorToMany0 5
  val1 <- get (Key k1 :: Key (New.AddConstructorToMany))
  Just (New.AddConstructorToMany1 1) @=? val1
  val2 <- get (Key k2 :: Key (New.AddConstructorToMany))
  Just (New.AddConstructorToMany2 "abc") @=? val2
  val0 <- get k0
  Just (New.AddConstructorToMany0 5) @=? val0

testReference :: (PersistBackend m, MonadControlIO m) => m ()
testReference = do
  migr (undefined :: Single (Single String))
  k <- insert $ Single (Single "abc")
  Just [valueKey'] <- queryRaw True "SELECT \"single\" FROM \"Single$Single$String\" WHERE id$=?" [toPrim k] firstRow
  assertExc "Foreign key must prevent deletion" $ deleteByKey (fromPrim valueKey' :: Key (Single String))

testMaybeReference :: (PersistBackend m, MonadControlIO m) => m ()
testMaybeReference = do
  migr (undefined :: Single (Maybe (Single String)))
  k <- insert $ Single (Just (Single "abc"))
  Just [valueKey'] <- queryRaw True "SELECT \"single\" FROM \"Single$Maybe$Single$String\" WHERE id$=?" [toPrim k] firstRow
  deleteByKey (fromPrim valueKey' :: Key (Single String))
  val' <- get k
  Just (Single Nothing) @=? val'
  
-- TODO: write test which inserts data before adding new columns

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

clean :: (PersistBackend m, MonadControlIO m) => m ()
clean = do
  executeRaw True "drop schema public cascade" []
  executeRaw True "create schema public" []

assertExc :: (PersistBackend m, MonadControlIO m) => String -> m a -> m ()
assertExc err m = do
  happened <- catch (m >> return False) $ \e -> const (return True) (e :: SomeException)
  unless happened $ liftIO (H.assertFailure err)
