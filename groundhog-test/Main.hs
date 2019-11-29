{-# LANGUAGE FlexibleContexts, TypeFamilies, CPP #-}

-- stack test --flag groundhog-test:mysql --flag groundhog-test:postgresql --flag groundhog-test:sqlite
module Main where

import GroundhogTest
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Core

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (forM_)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl)

#if WITH_SQLITE
import Database.Groundhog.Sqlite
#endif
#if WITH_POSTGRESQL
import Database.Groundhog.Postgresql
#endif
#if WITH_MYSQL
import Database.Groundhog.MySQL
#endif

main :: IO ()
main = do
#if WITH_POSTGRESQL
  let postgresql = [testGroup "Database.Groundhog.Postgresql" $ concatMap ($ runPSQL) [mkTestSuite, mkSqlTestSuite, postgresqlTestSuite] ++ mkTryTestSuite runPSQLWithConn]
      connString = "dbname=test user=test password=test host=localhost"
      runPSQL m = withPostgresqlConn connString . runDbConn $ m >> cleanPostgresql
      runPSQLWithConn m = withPostgresqlConn connString $ \conn -> m conn
#else
  let postgresql = []
#endif
#if WITH_SQLITE
  let sqlite = [testGroup "Database.Groundhog.Sqlite" $ concatMap ($ runSqlite)  [mkTestSuite, mkSqlTestSuite, sqliteTestSuite] ++ mkTryTestSuite runSqliteWithConn]
      runSqlite m = withSqliteConn ":memory:" . runDbConn $ m
      runSqliteWithConn m = withSqliteConn ":memory:" $ \conn -> m conn
#else
  let sqlite = []
#endif
#if WITH_MYSQL
  let mysql = [testGroup "Database.Groundhog.MySQL" $ concatMap ($ runMySQL) [mkTestSuite, mkSqlTestSuite, mysqlTestSuite] ++ mkTryTestSuite runMySQLWithConn]
      mySQLConnInfo = defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        }
      runMySQL m = withMySQLConn mySQLConnInfo . runDbConn $ m >> cleanMySQL
      runMySQLWithConn m = withMySQLConn mySQLConnInfo $ \conn -> m conn
#else
  let mysql = []
#endif
  defaultMain $ mysql ++ sqlite ++ postgresql

#if WITH_POSTGRESQL
cleanPostgresql :: (PersistBackend m, Conn m ~ Postgresql) => m ()
cleanPostgresql = do
  liftIO $ print "executing rollback"
  executeRaw True "rollback" []
  executeRaw True "begin" []
#endif

#if WITH_MYSQL
-- DDL statements are committed automatically so we cannot rollback them.
cleanMySQL :: (PersistBackend m, Conn m ~ MySQL) => m ()
cleanMySQL = do
  executeRaw True "SET FOREIGN_KEY_CHECKS = 0" []
  forM_ ["test", "myschema"] $ \schema -> do
    executeRaw True ("drop database if exists " ++ schema) []
    executeRaw True ("create database " ++ schema) []
  executeRaw True ("use test") []
  executeRaw True "SET FOREIGN_KEY_CHECKS = 1" []
#endif

mkSqlTestSuite :: (PersistBackend m, SqlDb (Conn m)) => (m () -> IO ()) -> [Test]
mkSqlTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testSelect", testSelect)
  , ("testCond", testCond)
  , ("testArith", testArith)
  , ("testProjectionSql", testProjectionSql)
  ]

mkTestSuite :: (PersistBackend m, MonadBaseControl IO m) => (m () -> IO ()) -> [Test]
mkTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testNumber", testNumber)
  , ("testPersistSettings", testPersistSettings)
  , ("testEmbedded", testEmbedded)
  , ("testSelectDistinct", testSelectDistinct)
  , ("testInsert", testInsert)
  , ("testMaybe", testMaybe)
  , ("testCount", testCount)
  , ("testUpdate", testUpdate)
  , ("testComparison", testComparison)
  , ("testEncoding", testEncoding)
  , ("testDelete", testDelete)
  , ("testDeleteBy", testDeleteBy)
  , ("testDeleteAll", testDeleteAll)
  , ("testReplaceSingle", testReplaceSingle)
  , ("testReplaceMulti", testReplaceMulti)
  , ("testReplaceBy", testReplaceBy)
  , ("testTuple", testTuple)
  , ("testTupleList", testTupleList)
  , ("testMigrateAddColumnSingle", testMigrateAddColumnSingle)
  , ("testMigrateAddUniqueConstraint", testMigrateAddUniqueConstraint)
  , ("testMigrateDropUniqueConstraint", testMigrateDropUniqueConstraint)
  , ("testMigrateAddUniqueIndex", testMigrateAddUniqueIndex)
  , ("testMigrateDropUniqueIndex", testMigrateDropUniqueIndex)
  , ("testMigrateAddDropNotNull", testMigrateAddDropNotNull)
  , ("testMigrateAddConstructorToMany", testMigrateAddConstructorToMany)
  , ("testMigrateChangeType", testMigrateChangeType)
--  , ("testLongNames", testLongNames)
  , ("testReference", testReference)
  , ("testMaybeReference", testMaybeReference)
  , ("testUniqueKey", testUniqueKey)
  , ("testForeignKeyUnique", testForeignKeyUnique)
  , ("testProjection", testProjection)
  , ("testKeyNormalization", testKeyNormalization)
  , ("testAutoKeyField", testAutoKeyField)
  , ("testStringAutoKey", testStringAutoKey)
  , ("testTime", testTime)
  , ("testPrimitiveData", testPrimitiveData)
  , ("testConverter", testConverter)
  , ("testNoColumns", testNoColumns)
  , ("testNoKeys", testNoKeys)
  , ("testJSON", testJSON)
  ]

mkTryTestSuite :: ( ExtractConnection conn conn
                  , PersistBackendConn conn
                  , TryConnectionManager conn
                  , MonadCatch m
                  , MonadFail m
                  , MonadBaseControl IO m
                  , MonadIO m)
               => ((conn -> m ()) -> IO ()) -> [Test]
mkTryTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testTryAction", testTryAction)
  ]

#if WITH_SQLITE
sqliteTestSuite :: (PersistBackend m, Conn m ~ Sqlite, MonadBaseControl IO m) => (m () -> IO ()) -> [Test]
sqliteTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testMigrateOrphanConstructors", testMigrateOrphanConstructors)
  , ("testSchemaAnalysis", testSchemaAnalysis)
  , ("testSchemaAnalysisSqlite", testSchemaAnalysisSqlite)
  , ("testListTriggersOnDelete", testListTriggersOnDelete)
  , ("testListTriggersOnUpdate", testListTriggersOnUpdate)
  ]
#endif

#if WITH_POSTGRESQL
postgresqlTestSuite :: (PersistBackend m, Conn m ~ Postgresql, MonadBaseControl IO m) => (m () -> IO ()) -> [Test]
postgresqlTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testGeometry", testGeometry)
  , ("testArrays", testArrays)
  , ("testSchemas", testSchemas)
  , ("testSchemaAnalysis", testSchemaAnalysis)
  , ("testSchemaAnalysisPostgresql", testSchemaAnalysisPostgresql)
  , ("testFloating", testFloating)
  , ("testListTriggersOnDelete", testListTriggersOnDelete)
  , ("testListTriggersOnUpdate", testListTriggersOnUpdate)
  , ("testSelectDistinctOn", testSelectDistinctOn)
  , ("testExpressionIndex", testExpressionIndex)
  , ("testHStore", testHStore)
  ]
#endif

#if WITH_MYSQL
mysqlTestSuite :: (PersistBackend m, Conn m ~ MySQL) => (m () -> IO ()) -> [Test]
mysqlTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testSchemas", testSchemas)
  , ("testSchemaAnalysis", testSchemaAnalysis)
  , ("testSchemaAnalysisMySQL", testSchemaAnalysisMySQL)
  , ("testFloating", testFloating)
--  , ("testListTriggersOnDelete", testListTriggersOnDelete)  -- fails due to MySQL bug #11472
--  , ("testListTriggersOnUpdate", testListTriggersOnUpdate)  -- fails due to MySQL bug #11472
  ]
#endif
