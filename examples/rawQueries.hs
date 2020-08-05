{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.Core (PersistEntity (..), PersistField (..), PrimitivePersistField (..))
import Database.Groundhog.Generic (mapAllRows, phantomDb)
import Database.Groundhog.Sqlite
import Database.Groundhog.TH

data SomeData = SomeData Int (Int, String) deriving (Show)

mkPersist
  defaultCodegenConfig
  [groundhog|
- entity: SomeData
|]

main = withSqliteConn ":memory:" $
  runDbConn $ do
    runMigration $ migrate (undefined :: SomeData)
    k1 <- insert $ SomeData 1 (2, "abc")
    k2 <- insert $ SomeData 10 (20, "def")
    proxy <- phantomDb

    -- PersistField instance for PersistEntity expects to receive its id, not values contained in data.
    -- So here we cannot use fromPersistValues to get data.
    xs1 <- queryRaw False "SELECT \"SomeData0\", \"someData1#val0\", \"someData1#val1\" FROM \"SomeData\" WHERE \"id\" = ? OR \"id\" = ?" [toPrimitivePersistValue k1, toPrimitivePersistValue k2] $ mapAllRows (fmap fst . fromPersistValues)
    liftIO $ print (xs1 :: [(Int, Int, String)])

    -- it will run 1 + N select queries to get data by id.
    xs2 <- queryRaw False "SELECT \"id\" FROM \"SomeData\" ORDER BY \"someData1#val0\" DESC" [] $ mapAllRows (fmap fst . fromPersistValues)
    liftIO $ print (xs2 :: [SomeData])

    -- function fromEntityPersistValues from PersistEntity expects constructor number (they start from 0) and the data contained in it.
    xs3 <- queryRaw False "SELECT \"SomeData0\", \"someData1#val0\", \"someData1#val1\" FROM \"SomeData\" WHERE \"id\" = ? OR \"id\" = ?" [toPrimitivePersistValue k1, toPrimitivePersistValue k2] $ mapAllRows (fmap fst . fromEntityPersistValues . (toPrimitivePersistValue (0 :: Int) :))
    liftIO $ print (xs3 :: [SomeData])

    -- the queries not supported by groundhog API may be run via raw SQL. If number of columns is too big (there are no instances PersistFields for tuples with more than 5 elements) you can nest tuples
    xs4 <- queryRaw False "SELECT s1.\"id\", s1.\"someData1#val1\", s2.\"id\", s2.\"someData1#val1\" FROM \"SomeData\" s1 INNER JOIN \"SomeData\" s2 ON s1.\"id\" <= s2.\"id\"" [] $ mapAllRows (fmap fst . fromPersistValues)
    liftIO $ print (xs4 :: [((String, String), (String, String))])
