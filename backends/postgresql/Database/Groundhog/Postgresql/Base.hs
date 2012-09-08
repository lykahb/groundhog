{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Database.Groundhog.Postgresql.Base
    ( Postgresql (..)
    , P (..)
    , escape
    , isSimple
    , getStatement
    , queryRaw'
    , proxy
    ) where

import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql.String
import Database.Groundhog.Instances ()

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.BuiltinTypes as PG
import qualified Database.PostgreSQL.Simple.Internal as PG
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.FromField as PGFF
import qualified Database.PostgreSQL.Simple.Types as PG
import Database.PostgreSQL.Simple.Ok (Ok (..))

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Control.Exception (throw)
import Control.Monad (liftM, forM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ask)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Int (Int64)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.LocalTime (localTimeToUTC, utc)

-- typical operations for connection: OPEN, BEGIN, COMMIT, ROLLBACK, CLOSE
newtype Postgresql = Postgresql PG.Connection

instance DbDescriptor Postgresql where
  type AutoKeyType Postgresql = Int64

-- It is used to escape table names and columns, which can include only symbols allowed in Haskell datatypes and '$' delimiter. We need it mostly to support names that coincide with SQL keywords
escape :: String -> String
escape s = '\"' : s ++ "\""

isSimple :: [a] -> Bool
isSimple [_] = True
isSimple _   = False
  
getStatement :: StringS -> PG.Query
getStatement sql = PG.Query $ T.encodeUtf8 $ T.pack $ fromStringS sql ""

  {-
queryRaw' :: (MonadBaseControl IO m, MonadIO m) => String -> [PersistValue] -> (RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m a) -> DbPersist Postgresql m a
queryRaw' query vals f = do
--  liftIO $ print $ query ++ show vals
  stmt <- getStatement query
  liftIO $ H.execute stmt (map pToSql vals)
  f $ liftIO $ do
    x <- H.fetchRow stmt
    return $ fmap (map pFromSql) x
-}

queryRaw' :: (MonadBaseControl IO m, MonadIO m) => StringS -> [PersistValue] -> (RowPopper (DbPersist Postgresql m) -> DbPersist Postgresql m a) -> DbPersist Postgresql m a
queryRaw' query vals f = do
  Postgresql conn <- DbPersist ask
  rawquery <- liftIO $ PG.formatQuery conn (getStatement query) (map P vals)
  -- Take raw connection
  (ret, rowRef, rowCount, getters) <- liftIO $ PG.withConnection conn $ \rawconn -> do
    -- Execute query
    mret <- LibPQ.exec rawconn rawquery
    case mret of
      Nothing -> do
        merr <- LibPQ.errorMessage rawconn
        fail $ case merr of
                 Nothing -> "Postgresql.withStmt': unknown error"
                 Just e  -> "Postgresql.withStmt': " ++ B8.unpack e
      Just ret -> do
        -- Check result status
        status <- LibPQ.resultStatus ret
        case status of
          LibPQ.TuplesOk -> return ()
          _ -> do
            msg <- LibPQ.resStatus status
            fail $ "Postgresql.withStmt': bad result status " ++
                   show status ++ " (" ++ show msg ++ ")"

        -- Get number and type of columns
        cols <- LibPQ.nfields ret
        getters <- forM [0..cols-1] $ \col -> do
          oid <- LibPQ.ftype ret col
          case PG.oid2builtin oid of
            Nothing -> fail $ "Postgresql.withStmt': could not " ++
                              "recognize Oid of column " ++
                              show (let LibPQ.Col i = col in i) ++
                              " (counting from zero)"
            Just bt -> return $ getGetter bt $
                       PG.Field ret col $
                       PG.builtin2typname bt
        -- Ready to go!
        rowRef   <- newIORef (LibPQ.Row 0)
        rowCount <- LibPQ.ntuples ret
        return (ret, rowRef, rowCount, getters)

  f $ liftIO $ do
    row <- atomicModifyIORef rowRef (\r -> (r+1, r))
    if row == rowCount
      then return Nothing
      else liftM Just $ forM (zip getters [0..]) $ \(getter, col) -> do
        mbs <-  {-# SCC "getvalue'" #-} LibPQ.getvalue' ret row col
        case mbs of
          Nothing -> return PersistNull
          Just bs -> bs `seq` case getter mbs of
            Errors (exc:_) -> throw exc
            Errors [] -> error "Got an Errors, but no exceptions"
            Ok v  -> return v
    
-- | Avoid orphan instances.
newtype P = P PersistValue

instance PGTF.ToField P where
    toField (P (PersistString t))        = PGTF.toField t
    toField (P (PersistByteString bs)) = PGTF.toField (PG.Binary bs)
    toField (P (PersistInt64 i))       = PGTF.toField i
    toField (P (PersistDouble d))      = PGTF.toField d
    toField (P (PersistBool b))        = PGTF.toField b
    toField (P (PersistDay d))         = PGTF.toField d
    toField (P (PersistTimeOfDay t))   = PGTF.toField t
    toField (P (PersistUTCTime t))     = PGTF.toField t
    toField (P (PersistZonedTime (ZT t))) = PGTF.toField t
    toField (P PersistNull)            = PGTF.toField PG.Null

type Getter a = PG.Field -> Maybe ByteString -> Ok a

convertPV :: PGFF.FromField a => (a -> b) -> Getter b
convertPV f = (fmap f .) . PGFF.fromField

-- FIXME: check if those are correct and complete.
getGetter :: PG.BuiltinType -> Getter PersistValue
getGetter PG.Bool                  = convertPV PersistBool
getGetter PG.ByteA                 = convertPV (PersistByteString . unBinary)
getGetter PG.Char                  = convertPV PersistString
getGetter PG.Name                  = convertPV PersistString
getGetter PG.Int8                  = convertPV PersistInt64
getGetter PG.Int2                  = convertPV PersistInt64
getGetter PG.Int4                  = convertPV PersistInt64
getGetter PG.Text                  = convertPV PersistString
getGetter PG.Xml                   = convertPV PersistString
getGetter PG.Float4                = convertPV PersistDouble
getGetter PG.Float8                = convertPV PersistDouble
getGetter PG.AbsTime               = convertPV PersistUTCTime
getGetter PG.RelTime               = convertPV PersistUTCTime
getGetter PG.Money                 = convertPV PersistDouble
getGetter PG.BpChar                = convertPV PersistString
getGetter PG.VarChar               = convertPV PersistString
getGetter PG.Date                  = convertPV PersistDay
getGetter PG.Time                  = convertPV PersistTimeOfDay
getGetter PG.Timestamp             = convertPV (PersistUTCTime . localTimeToUTC utc)
getGetter PG.TimestampTZ           = convertPV (PersistZonedTime . ZT)
getGetter PG.Bit                   = convertPV PersistInt64
getGetter PG.VarBit                = convertPV PersistInt64
getGetter PG.Numeric               = convertPV (PersistDouble . fromRational)
getGetter PG.Void                  = \_ _ -> Ok PersistNull
getGetter other   = error $ "Postgresql.getGetter: type " ++
                            show other ++ " not supported."

unBinary :: PG.Binary a -> a
unBinary (PG.Binary x) = x

proxy :: Proxy Postgresql
proxy = error "Proxy Postgresql"
