{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleContexts, FlexibleInstances, RankNTypes #-}
module Main where

import qualified Database.Groundhog.Core as G
import qualified Database.Groundhog.Sqlite as G
import qualified Database.Groundhog.Postgresql as G
import qualified Database.Groundhog.TH as G
import Database.Groundhog.TH (groundhog)

import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Conduit (ResourceT, runResourceT)
import qualified Database.Persist as P
import Database.Persist (PersistEntityBackend)
import qualified Database.Persist.GenericSql.Raw as P
import qualified Database.Persist.Sqlite as P
import qualified Database.Persist.Postgresql as P
import qualified Database.Persist.TH as P
import Database.Persist.TH (persist)

--import Database.Esqueleto as E

import Criterion.Main
import Criterion.Config
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (runReaderT)

data GPerson = GPerson { name :: String, age :: Int, height :: Int }
G.mkPersist G.defaultCodegenConfig [groundhog|
- entity: GPerson
|]

P.share [P.mkPersist P.sqlSettings, P.mkMigrate "migrateAll"] [persist|
PPerson
    name String
    age Int
    height Int
|]

myConfig = defaultConfig {
             cfgReport = ljust "PostgreSQLBench.html"
           , cfgPerformGC = ljust True
           }

gPerson = GPerson "John Doe" 23 180
gCond = NameField G.==. ("abc" :: String) G.&&. AgeField G.==. (40 :: Int) G.&&. HeightField G.==. (160 :: Int)

pPerson = PPerson "John Doe" 23 180
pCond = [PPersonName P.==. "abc", PPersonAge P.==. 40, PPersonHeight P.==. 160]

gMigrate :: (G.PersistBackend m, MonadIO m) => m () -> m (G.Key GPerson G.BackendSpecific)
gMigrate truncate = G.runMigration G.defaultMigrationLogger (G.migrate gPerson) >> truncate >> G.insert gPerson
pMigrate :: P.SqlPersist (NoLoggingT (ResourceT IO)) () -> P.SqlPersist (NoLoggingT (ResourceT IO)) (P.Key PPerson)
pMigrate truncate = P.runMigration migrateAll >> truncate >> P.insert pPerson

-- open transaction to reduce execution time on the DB side
eachStatementInTransaction :: Bool
eachStatementInTransaction = False

-- operatons are replicated to reduce influence of running a monad on the actual library and database performance measurements
numberOfOperations :: Int
numberOfOperations = 10

main = 
  G.withSqliteConn ":memory:" $ \gConn -> 
  P.withSqliteConn ":memory:" $ \pConn -> do
    gKey <- G.runSqliteConn (gMigrate $ return ()) gConn
    pKey <- runResourceT $ runNoLoggingT $ P.runSqlConn (pMigrate $ return ()) pConn
{-
  G.withPostgresqlConn "dbname=test user=test password=test host=localhost" $ \gConn ->
  P.withPostgresqlConn "dbname=test user=test password=test host=localhost" $ \pConn -> do
    gKey <- G.runPostgresqlConn (gMigrate $ G.executeRaw False "truncate table \"GPerson\"" []) gConn
    pKey <- runResourceT $ runNoLoggingT $ P.runSqlConn (pMigrate $ P.execute "truncate table \"PPerson\"" []) pConn
-}
    unless eachStatementInTransaction $ do
      G.runDbPersist (G.executeRaw False "BEGIN" []) gConn
      runResourceT $ runNoLoggingT $ runReaderT ((\(P.SqlPersist m) -> m) $ P.execute "BEGIN" []) pConn

    let mkBench :: (forall m . G.PersistBackend m => m a1) -> P.SqlPersist (NoLoggingT (ResourceT IO)) a2 -> [Benchmark]
        mkBench gm pm = [bench "groundhog" $ whnfIO $ runSqlite gm, bench "persistent" $ whnfIO $ runPersistent pm] where
          (runSqlite, runPersistent) = if eachStatementInTransaction
            then (\gm -> G.runSqliteConn (replicateM_ numberOfOperations gm) gConn, \pm -> runResourceT $ runNoLoggingT $ P.runSqlConn (replicateM_ numberOfOperations pm) pConn)
            else (\gm -> G.runDbPersist (replicateM_ numberOfOperations gm) gConn, \(P.SqlPersist pm) -> runResourceT $ runNoLoggingT $ runReaderT (replicateM_ numberOfOperations pm) pConn)
    defaultMainWith myConfig (return ())
      [ bgroup "get" $ mkBench (G.get gKey) (P.get pKey)
--      , bgroup "get" [bench "esqueleto" $ whnfIO $  runPers (E.select $ E.from $ \p -> E.where_ (p ^. PPersonId ==. val pKey) >> return p)]
      , bgroup "replace" $ mkBench (G.replace gKey gPerson) (P.replace pKey pPerson)
      , bgroup "select" $ mkBench (G.project (G.AutoKeyField, GPersonConstructor) gCond) (P.selectList pCond [])
      , bgroup "updateByKey" $ mkBench (G.update [NameField G.=. ("abc" :: String)] $ G.AutoKeyField G.==. gKey) (P.update pKey [PPersonName P.=. "abc"])
      , bgroup "updateWhere" $ mkBench (G.update [NameField G.=. ("abc" :: String)] gCond) (P.updateWhere pCond [PPersonName P.=. "abc"])
      , bgroup "count" $ mkBench (G.count gCond) (P.count pCond)
      , bgroup "deleteByKey" $ mkBench (G.deleteByKey gKey) (P.delete pKey)
      , bgroup "deleteWhere" $ mkBench (G.delete gCond) (P.deleteWhere pCond)
      , bgroup "insert" $ mkBench (G.insert gPerson) (P.insert pPerson)
      , bgroup "insert_" $ mkBench (G.insert_ gPerson) (P.insert_ pPerson)
      ]
