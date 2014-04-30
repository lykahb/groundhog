{-# LANGUAGE FlexibleContexts, TypeFamilies, CPP #-}

import GroundhogTest
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Core

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (forM_)
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
  let postgresql = [testGroup "Database.Groundhog.Postgresql" $ concatMap ($ runPSQL) [mkTestSuite, mkSqlTestSuite, postgresqlTestSuite]]
      runPSQL m = withPostgresqlConn "dbname=test user=test password=test host=localhost" . runDbConn $ cleanPostgresql >> m
#else
  let postgresql = []
#endif
#if WITH_SQLITE
  let sqlite = [testGroup "Database.Groundhog.Sqlite" $ concatMap ($ runSqlite)  [mkTestSuite, mkSqlTestSuite, sqliteTestSuite]]
      runSqlite m = withSqliteConn ":memory:" . runDbConn $ m
#else
  let sqlite = []
#endif
#if WITH_MYSQL
  let mysql = [testGroup "Database.Groundhog.MySQL" $ concatMap ($ runMySQL) [mkTestSuite, mkSqlTestSuite, mysqlTestSuite]]
      mySQLConnInfo = defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        }
      runMySQL m = withMySQLConn mySQLConnInfo . runDbConn $ cleanMySQL >> m
#else
  let mysql = []
#endif
  defaultMain $ mysql ++ sqlite ++ postgresql

#if WITH_POSTGRESQL
cleanPostgresql :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist Postgresql m ()
cleanPostgresql = forM_ ["public", "myschema"] $ \schema -> do
  executeRaw True ("drop schema if exists " ++ schema ++ " cascade") []
  executeRaw True ("create schema " ++ schema) []
  executeRaw True ("alter schema " ++ schema ++ " owner to test") []
#endif

#if WITH_MYSQL
cleanMySQL :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => DbPersist MySQL m ()
cleanMySQL = do
  executeRaw True "SET FOREIGN_KEY_CHECKS = 0" []
  forM_ ["test", "myschema"] $ \schema -> do
    executeRaw True ("drop database if exists " ++ schema) []
    executeRaw True ("create database " ++ schema) []
  executeRaw True ("use test") []
  executeRaw True "SET FOREIGN_KEY_CHECKS = 1" []
#endif

mkSqlTestSuite :: (PersistBackend m, MonadBaseControl IO m, MonadIO m, db ~ PhantomDb m, SqlDb db) => (m () -> IO ()) -> [Test]
mkSqlTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testSelect", testSelect)
  , ("testCond", testCond)
  , ("testArith", testArith)
  , ("testProjectionSql", testProjectionSql)
  ]

mkTestSuite :: (PersistBackend m, MonadBaseControl IO m, MonadIO m) => (m () -> IO ()) -> [Test]
mkTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testNumber", testNumber)
  , ("testPersistSettings", testPersistSettings)
  , ("testEmbedded", testEmbedded)
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
  , ("testTime", testTime)
  , ("testPrimitiveData", testPrimitiveData)
  ]

#if WITH_SQLITE
sqliteTestSuite :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => (DbPersist Sqlite m () -> IO ()) -> [Test]
sqliteTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testMigrateOrphanConstructors", testMigrateOrphanConstructors)
  , ("testSchemaAnalysis", testSchemaAnalysis)
  , ("testSchemaAnalysisSqlite", testSchemaAnalysisSqlite)
  , ("testListTriggersOnDelete", testListTriggersOnDelete)
  , ("testListTriggersOnUpdate", testListTriggersOnUpdate)
  ]
#endif

#if WITH_POSTGRESQL
postgresqlTestSuite :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => (DbPersist Postgresql m () -> IO ()) -> [Test]
postgresqlTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testGeometry", testGeometry)
  , ("testArrays", testArrays)
  , ("testSchemas", testSchemas)
  , ("testSchemaAnalysis", testSchemaAnalysis)
  , ("testSchemaAnalysisPostgresql", testSchemaAnalysisPostgresql)
  , ("testFloating", testFloating)
  , ("testListTriggersOnDelete", testListTriggersOnDelete)
  , ("testListTriggersOnUpdate", testListTriggersOnUpdate)
  ]
#endif

#if WITH_MYSQL
mysqlTestSuite :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => (DbPersist MySQL m () -> IO ()) -> [Test]
mysqlTestSuite run = map (\(name, func) -> testCase name $ run func)
  [ ("testSchemas", testSchemas)
  , ("testSchemaAnalysis", testSchemaAnalysis)
  , ("testSchemaAnalysisMySQL", testSchemaAnalysisMySQL)
  , ("testFloating", testFloating)
--  , ("testListTriggersOnDelete", testListTriggersOnDelete)  -- fails due to MySQL bug #11472
--  , ("testListTriggersOnUpdate", testListTriggersOnUpdate)  -- fails due to MySQL bug #11472
  ]
#endif
