{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- @
-- import Database.Groundhog.Sqlite
-- import Database.Groundhog.Pipes
-- import Pipes
-- import qualified Pipes.Prelude as P
-- import Pipes.Safe (runSafeT)
--
-- main :: IO ()
-- main = withSqliteConn ":memory:" $ runDbConn $ do
--   executeRaw True \"CREATE TABLE example(value INTEGER)\" []
--   executeRaw True \"INSERT INTO example (value) VALUES (1)\" []
--   executeRaw True \"INSERT INTO example (value) VALUES (2)\" []
--   runSafeT $ runEffect $ queryRawStream True \"SELECT * FROM example\" [] >-> P.print
-- @
module Database.Groundhog.Pipes
  ( selectStream,
    selectAllStream,
    projectStream,
    queryRawStream,
  )
where

import Control.Monad.Trans.Class (lift)
import Database.Groundhog.Core hiding (PersistBackendConn (..))
import qualified Database.Groundhog.Core as Core
import Pipes (MonadIO (..), Producer', yield)
import Pipes.Safe

fromStream :: (MonadIO m, MonadSafe m) => m (RowStream a) -> Producer' a m ()
fromStream stream = do
  (next, close) <- lift stream
  let go = do
        a <- liftIO next
        case a of
          Just a' -> yield a' >> go
          Nothing -> pure ()
  case close of
    Nothing -> go
    Just close' -> go `finally` liftIO close'

selectStream ::
  (PersistBackend m, MonadSafe m, PersistEntity v, EntityConstr v c, HasSelectOptions opts (Conn m) (RestrictionHolder v c)) =>
  opts ->
  Producer' v m ()
selectStream opts = fromStream $ Core.selectStream opts

selectAllStream :: (MonadSafe m, PersistBackend m, PersistEntity v) => Producer' (AutoKey v, v) m ()
selectAllStream = fromStream Core.selectAllStream

projectStream ::
  (MonadSafe m, PersistBackend m, PersistEntity v, EntityConstr v c, Projection' p (Conn m) (RestrictionHolder v c) a, HasSelectOptions opts (Conn m) (RestrictionHolder v c)) =>
  p ->
  opts ->
  Producer' a m ()
projectStream p opts = fromStream $ Core.projectStream p opts

queryRawStream ::
  (MonadSafe m, PersistBackend m) =>
  -- | keep in cache
  Bool ->
  -- | query
  String ->
  -- | positional parameters
  [PersistValue] ->
  Producer' [PersistValue] m ()
queryRawStream cache query ps = fromStream $ Core.queryRaw cache query ps

instance PersistBackend m => PersistBackend (SafeT m) where
  type Conn (SafeT m) = Conn m
  getConnection = lift getConnection
