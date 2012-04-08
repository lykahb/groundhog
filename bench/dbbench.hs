{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes #-}

import Control.Monad
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data Person = Person {name :: String, age :: Int, height :: Int} deriving (Eq, Show)

mkPersist fieldNamingStyle [groundhog|
- entity: Person
|]

-- 0.0.1.1 sqlite
-- 100000 $ insert ~1.54
-- 100000 $ get ~0.98
-- postgresql
-- 10000  $ insert ~6.5
-- 10000  $ get ~6
main :: IO ()
main = withSqliteConn ":memory:" $ runSqliteConn $ do
  runMigration silentMigrationLogger $ migrate (undefined :: Person)
  let person = Person "abc" 22 180
  k <- insert $ person
  replicateM_ 1000000 $ get k
--  replicateM_ 100000 $ insert person
