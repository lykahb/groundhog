{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (LoggingT, MonadLogger (..), runStdoutLoggingT)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import Data.Pool
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Sqlite

main :: IO ()
main = withSqlitePool ":memory:" 5 $ \pconn -> do
  let runMyMonadDB :: MyMonad a -> IO a
      runMyMonadDB = flip runReaderT (ApplicationState pconn) . runStdoutLoggingT . runMyMonad
  runMyMonadDB sqliteDbAction

-- It is connection agnostic (runs both with Sqlite and Pool Sqlite)
sqliteDbAction :: (PersistBackend m, Conn m ~ Sqlite) => m ()
sqliteDbAction = do
  -- here can be web business logics
  runDb $ do
    let runAndShow sql = queryRaw False sql [] >>= firstRow >>= liftIO . print
    runAndShow "select 'Groundhog embedded in arbitrary monadic context'"

-- This can be Snaplet in Snap or foundation datatype in Yesod.
data ApplicationState = ApplicationState {getConnPool :: Pool Sqlite}

-- -- This instance extracts connection from our application state
-- instance ExtractConnection ApplicationState Sqlite where
--   extractConn f app = extractConn f (getConnPool app)

-- This can be any application monad like Handler in Snap or GHandler in Yesod
newtype MyMonad a = MyMonad {runMyMonad :: LoggingT (ReaderT ApplicationState IO) a}
  deriving (Applicative, Functor, Monad, MonadReader ApplicationState, MonadIO, MonadLogger)

instance MonadFail MyMonad where
  fail = error

instance MonadBase IO MyMonad where
  liftBase = liftIO

instance PersistBackend MyMonad where
  type Conn MyMonad = Sqlite
  getConnection = liftM getConnPool ask
