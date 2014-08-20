{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts #-}
import Database.Groundhog.Core (ConnectionManager(..))
import Database.Groundhog.Generic
import Database.Groundhog.Sqlite

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger(..), LoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Reader (MonadReader(..))
import Data.Pool

main :: IO ()
main = withSqlitePool ":memory:" 5 $ \pconn ->
    flip runReaderT (ApplicationState pconn) . runStdoutLoggingT . runMyMonad $ sqliteDbAction

-- It is connection agnostic (runs both with Sqlite and Pool Sqlite)
sqliteDbAction :: (MonadBaseControl IO m, HasConn m cm Sqlite) => m ()
sqliteDbAction = do
  -- here can be web business logics
  runDb $ do
    let runAndShow sql = queryRaw False sql [] (>>= liftIO . print)
    runAndShow "select 'Groundhog embedded in arbitrary monadic context'"
    withSavepoint "savepoint_name" $ do
      runAndShow "select 'SQL inside savepoint'"

-- It is like Snaplet in Snap or foundation datatype in Yesod.
data ApplicationState = ApplicationState { getConnPool :: Pool Sqlite }

-- This instance extracts connection from our application state
instance ConnectionManager ApplicationState Sqlite where
  withConn f app = withConn f (getConnPool app)
  withConnNoTransaction f app = withConnNoTransaction f (getConnPool app)

-- This can be any application monad like Handler in Snap or GHandler in Yesod
newtype MyMonad a = MyMonad { runMyMonad :: LoggingT (ReaderT ApplicationState IO) a }
  deriving (Applicative, Functor, Monad, MonadReader ApplicationState, MonadIO, MonadLogger)

instance MonadBase IO MyMonad where
  liftBase = liftIO

instance MonadBaseControl IO MyMonad where
  newtype StM MyMonad a = StMMyMonad { unStMMyMonad :: StM (LoggingT (ReaderT ApplicationState IO)) a }
  liftBaseWith f = MyMonad (liftBaseWith (\run -> f (liftM StMMyMonad . run . runMyMonad)))
  restoreM = MyMonad . restoreM . unStMMyMonad
