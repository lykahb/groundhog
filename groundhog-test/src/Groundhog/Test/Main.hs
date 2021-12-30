{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- stack test --flag groundhog-test:mysql --flag groundhog-test:postgresql --flag groundhog-test:sqlite
import Data.Pool (withResource)
import Database.Groundhog.Core
import qualified Groundhog.Test.Specs.GeneralSpec as GeneralSpec
import qualified Groundhog.Test.Specs.MigrationSpec as MigrationSpec
import Test.Hspec

#if WITH_SQLITE
import Database.Groundhog.Sqlite
import qualified Groundhog.Test.Specs.SqliteSpec as SqliteSpec
#endif
#if WITH_POSTGRESQL
import Database.Groundhog.Postgresql
import qualified Groundhog.Test.Specs.PostgresqlSpec as PostgresqlSpec
#endif
#if WITH_MYSQL
import Database.Groundhog.MySQL
import qualified Groundhog.Test.Specs.MySQLSpec as MySQLSpec
#endif

main :: IO ()
main = hspec $ do

#if WITH_SQLITE
  around (withSqliteConn ":memory:") $
    describe "Database.Groundhog.Sqlite" $ do
      describe "General" $ do
        GeneralSpec.spec
      describe "Migrations" MigrationSpec.spec
      describe "Sqlite" SqliteSpec.spec
#endif

#if WITH_POSTGRESQL
  let connString = "dbname=test user=test password=test host=localhost"
  psqlPool <- runIO $ createPostgresqlPool connString 1
  around (\m -> withResource psqlPool (m >> runDbConn PostgresqlSpec.cleanPostgresql)) $
    describe "Database.Groundhog.Postgresql" $ do
      describe "General" $ do
        GeneralSpec.spec
        GeneralSpec.floatingSpec
      describe "Migrations" $ do
        MigrationSpec.spec
        MigrationSpec.migrateAnotherSchemaSpec
      describe "Postgresql" PostgresqlSpec.spec
#endif

#if WITH_MYSQL
  let mySQLConnInfo = defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        }
  mysqlPool <- runIO $ createMySQLPool mySQLConnInfo 1
  around (\m -> withResource mysqlPool (m >> runDbConn MySQLSpec.cleanMySQL)) $
    describe "Database.Groundhog.MySQL" $ do
      describe "General" $ do
        GeneralSpec.spec
        GeneralSpec.floatingSpec
      describe "Migrations" $ do
        MigrationSpec.spec
        MigrationSpec.migrateAnotherSchemaSpec
      describe "MySQL" MySQLSpec.spec
#endif
