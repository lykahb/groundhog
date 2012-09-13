{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleContexts, FlexibleInstances, RankNTypes #-}
module Main where

import qualified Database.Groundhog.Core as G
import qualified Database.Groundhog.Sqlite as G
--import qualified Database.Groundhog.Postgresql as G
import qualified Database.Groundhog.TH as G
import Database.Groundhog.TH (groundhog)

import qualified Database.Persist as P
import Database.Persist (PersistEntityBackend)
import qualified Database.Persist.GenericSql.Raw as P
import qualified Database.Persist.Sqlite as P
--import qualified Database.Persist.Postgresql as P
import qualified Database.Persist.TH as P
import Database.Persist.TH (persist)

import Criterion.Main
import Criterion.Config
import Control.Monad (replicateM_)
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
             cfgReport = ljust "SqliteBench.html"
           , cfgPerformGC = ljust True
           }

gPerson = GPerson "John Doe" 23 180
gCond = NameField G.==. ("abc" :: String) G.&&. AgeField G.==. (40 :: Int) G.&&. HeightField G.==. (160 :: Int)

pPerson = PPerson "John Doe" 23 180
pCond = [PPersonName P.==. "abc", PPersonAge P.==. 40, PPersonHeight P.==. 160]

gMigrate :: (G.PersistBackend m, MonadIO m) => m () -> m (G.Key GPerson G.BackendSpecific)
gMigrate truncate = G.runMigration G.defaultMigrationLogger (G.migrate gPerson) >> truncate >> G.insert gPerson
pMigrate :: P.SqlPersist IO () -> P.SqlPersist IO (P.Key P.SqlPersist PPerson)
pMigrate truncate = P.runMigration migrateAll >> truncate >> P.insert pPerson

main = 
  G.withSqliteConn ":memory:" $ \gConn -> 
  P.withSqliteConn ":memory:" $ \pConn -> do
    gKey <- G.runSqliteConn (gMigrate $ return ()) gConn
    pKey <- P.runSqlConn (pMigrate $ return ()) pConn
{-
  G.withPostgresqlConn "dbname=test user=test password=test host=localhost" $ \gConn ->
  P.withPostgresqlConn "dbname=test user=test password=test host=localhost" $ \pConn -> do
    gKey <- G.runPostgresqlConn (gMigrate $ G.executeRaw False "truncate table \"GPerson\"" []) gConn
    pKey <- P.runSqlConn (pMigrate $ P.execute "truncate table \"PPerson\"" []) pConn
-}
-- replicateM_ is used to reduce influence of running a monad on the actual database performance
-- open transaction to reduce execution time on the DB side
    G.runDbPersist (G.executeRaw False "BEGIN" []) gConn
    runReaderT ((\(P.SqlPersist m) -> m) $ P.execute "BEGIN" []) pConn
    let mkBench :: (forall m . G.PersistBackend m => m a1) -> P.SqlPersist IO a2 -> [Benchmark]
        mkBench gm (P.SqlPersist pm) = [bench "groundhog" $ whnfIO $ G.runDbPersist (replicateM_ 10 gm) gConn, bench "persistent" $ whnfIO $ runReaderT (replicateM_ 10 pm) pConn]
    defaultMainWith myConfig (return ())
      [ bgroup "get" $ mkBench (G.get gKey) (P.get pKey)
      , bgroup "replace" $ mkBench (G.replace gKey gPerson) (P.replace pKey pPerson)
      , bgroup "select" $ mkBench (G.project (G.AutoKeyField, GPersonConstructor) gCond) (P.selectList pCond [])
      , bgroup "updateByKey" $ mkBench (G.update [NameField G.=. ("abc" :: String)] $ G.AutoKeyField G.==. gKey) (P.update pKey [PPersonName P.=. "abc"])
      , bgroup "updateWhere" $ mkBench (G.update [NameField G.=. ("abc" :: String)] gCond) (P.updateWhere pCond [PPersonName P.=. "abc"])
      , bgroup "count" $ mkBench (G.count gCond) (P.count pCond)
      , bgroup "deleteByKey" $ mkBench (G.deleteByKey gKey) (P.delete pKey)
      , bgroup "deleteWhere" $ mkBench (G.delete gCond) (P.deleteWhere pCond)
      , bgroup "insert" $ mkBench (G.insert gPerson) (P.insert pPerson)
      ]
