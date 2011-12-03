{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell #-}

import Control.Monad
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data Person = Person {name :: String, age :: Int, height :: Int} deriving (Eq, Show)

deriveEntity ''Person Nothing

-- 0.0.1.1
-- 100000 $ insert ~1.54
-- 100000 $ get ~0.98
main :: IO ()
main = withSqliteConn ":memory:" $ runSqliteConn $ do
  runMigration silentMigrationLogger $ migrate (undefined :: Person)
  k <- insert $ Person "abc" 22 180
  replicateM_ 100000 $ get k
--  replicateM_ 100000 $ insert $ Person "abc" 22 180
