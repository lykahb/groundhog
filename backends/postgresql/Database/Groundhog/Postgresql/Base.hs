{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Database.Groundhog.Postgresql.Base
    ( Postgresql (..)
    , escape
    , isSimple
    , mapAllRows
    , getStatement
    , queryRaw'
    , pToSql
    , pFromSql
    , proxy
    ) where

import Database.Groundhog.Core
import Database.Groundhog.Instances ()

import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as H

import Control.Monad (liftM, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ask)
import Data.Int (Int64)
import Data.IORef
import qualified Data.Map as Map

import Data.Time.LocalTime (localTimeToUTC, utc)

-- typical operations for connection: OPEN, BEGIN, COMMIT, ROLLBACK, CLOSE
data Postgresql = Postgresql H.Connection (IORef (Map.Map String H.Statement))

instance DbDescriptor Postgresql where
  type AutoKeyType Postgresql = Int64

-- It is used to escape table names and columns, which can include only symbols allowed in Haskell datatypes and '$' delimiter. We need it mostly to support names that coincide with SQL keywords
escape :: String -> String
escape s = '\"' : s ++ "\""

isSimple :: [a] -> Bool
isSimple [_] = True
isSimple _   = False

mapAllRows :: Monad m => ([PersistValue] -> m a) -> RowPopper m -> m [a]
mapAllRows f pop = go where
  go = pop >>= maybe (return []) (f >=> \a -> liftM (a:) go)
  
getStatement :: MonadIO m => String -> DbPersist Postgresql m H.Statement
getStatement sql = do
  Postgresql conn _ <- DbPersist ask
  liftIO $ H.prepare conn sql
  
queryRaw' :: (MonadBaseControl IO m, MonadIO m) => String -> [PersistValue] -> (RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m a) -> DbPersist Postgresql m a
queryRaw' query vals f = do
--  liftIO $ print $ query ++ show vals
  stmt <- getStatement query
  liftIO $ H.execute stmt (map pToSql vals)
  f $ liftIO $ do
    x <- H.fetchRow stmt
    return $ fmap (map pFromSql) x
    
pToSql :: PersistValue -> H.SqlValue
pToSql (PersistString t) = H.SqlString t
pToSql (PersistByteString bs) = H.SqlByteString bs
pToSql (PersistInt64 i) = H.SqlInt64 i
pToSql (PersistDouble d) = H.SqlDouble d
pToSql (PersistBool b) = H.SqlBool b
pToSql (PersistDay d) = H.SqlLocalDate d
pToSql (PersistTimeOfDay t) = H.SqlLocalTimeOfDay t
pToSql (PersistUTCTime t) = H.SqlUTCTime t
pToSql (PersistZonedTime (ZT t)) = H.SqlZonedTime t
pToSql PersistNull = H.SqlNull

pFromSql :: H.SqlValue -> PersistValue
pFromSql (H.SqlString s) = PersistString s
pFromSql (H.SqlByteString bs) = PersistByteString bs
pFromSql (H.SqlWord32 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlWord64 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInt32 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInt64 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInteger i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlChar c) = PersistInt64 $ fromIntegral $ fromEnum c
pFromSql (H.SqlBool b) = PersistBool b
pFromSql (H.SqlDouble b) = PersistDouble b
pFromSql (H.SqlRational b) = PersistDouble $ fromRational b
pFromSql (H.SqlLocalDate d) = PersistDay d
pFromSql (H.SqlLocalTimeOfDay d) = PersistTimeOfDay d
pFromSql (H.SqlUTCTime d) = PersistUTCTime d
pFromSql (H.SqlZonedTime d) = PersistZonedTime (ZT d)
pFromSql H.SqlNull = PersistNull
pFromSql (H.SqlLocalTime d) = PersistUTCTime $ localTimeToUTC utc d
pFromSql x = PersistString $ H.fromSql x -- FIXME

proxy :: Proxy Postgresql
proxy = error "Proxy Postgresql"
