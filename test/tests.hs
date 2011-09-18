{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, RankNTypes #-}

import Control.Monad(replicateM_, liftM)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.IO.Control (MonadControlIO)
import Database.Groundhog.TH
import Database.Groundhog.Sqlite
import Database.Groundhog.Postgresql
import Data.Int
import Data.Word
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

data User a = U1 { foo :: Int} | U2 { bar :: Maybe String, asc :: Int64, add :: a} | U3 deriving (Eq, Show)
data Person = Person {name :: String, age :: Int, height :: Int} deriving (Eq, Show)
data Number = Number { int :: Int, int8 :: Int8, word8 :: Word8, int16 :: Int16, word16 :: Word16, int32 :: Int32, word32 :: Word32, int64 :: Int64, word64 :: Word64 } deriving (Eq, Show)

deriveEntity ''User Nothing
deriveEntity ''Person Nothing
deriveEntity ''Number Nothing

main :: IO ()
main = do
  let runSqlite = withSqliteConn "testdb" . runSqliteConn
  let runPSQL = withPostgresqlConn "dbname=mydb user=boris password=12345" . runPostgresqlConn
  runSqlite setup >> runPSQL setup
  defaultMain [ mkTestSuite "Database.Groundhog.Sqlite" $ runSqlite . (clearAll >>)
              , mkTestSuite "Database.Groundhog.Postgresql" $ runPSQL . (clearAll >>)
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
--  printMigration m
  executeMigration silentMigrationLogger m

mkTestSuite :: (PersistBackend m, MonadIO m) => String -> (m () -> IO ()) -> Test
mkTestSuite label run = testGroup label $
  [ testCase "testNumber" $ run testNumber
  , testCase "testInsert" $ run testInsert
  , testCase "testCount" $ run testCount
  ]

runTest :: (PersistBackend m, MonadIO m) => m () -> m ()
runTest m = m >> clearAll

(@=?) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
expected @=? actual = liftIO $ expected H.@=? actual

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

clearUsers :: (PersistBackend m, MonadIO m) => m ()
clearUsers = do
  executeRaw True "DELETE FROM User$Tuple2$$String$Maybe$User$String$U1" []
  executeRaw True "DELETE FROM User$Tuple2$$String$Maybe$User$String$U2" []
  executeRaw True "DELETE FROM User$Tuple2$$String$Maybe$User$String$U3" []

  executeRaw True "DELETE FROM User$List$$User$String$U1" []
  executeRaw True "DELETE FROM User$List$$User$String$U2" []
  executeRaw True "DELETE FROM User$List$$User$String$U3" []

  executeRaw True "DELETE FROM User$Maybe$String$U1" []
  executeRaw True "DELETE FROM User$Maybe$String$U2" []
  executeRaw True "DELETE FROM User$Maybe$String$U3" []

  executeRaw True "DELETE FROM User$String$U1" []
  executeRaw True "DELETE FROM User$String$U2" []
  executeRaw True "DELETE FROM User$String$U3" []

clearPersons :: (PersistBackend m, MonadIO m) => m ()
clearPersons = executeRaw True "DELETE FROM Person" []

clearNumbers :: (PersistBackend m, MonadIO m) => m ()
clearNumbers = executeRaw True "DELETE FROM Number" []

clearAll :: (PersistBackend m, MonadIO m) => m ()
clearAll = clearUsers >> clearPersons >> clearNumbers
