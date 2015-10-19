{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances #-}
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (unpack)
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.TH
import Database.Groundhog.Postgresql

data Point = Point { pointX :: Int, pointY :: Int } deriving Show

-- PostgreSQL keeps point in format "(x,y)". This instance relies on the correspondence between Haskell tuple format and PostgreSQL point format.
instance PrimitivePersistField Point where
  toPrimitivePersistValue p (Point x y) = toPrimitivePersistValue p $ show (x, y)
  fromPrimitivePersistValue p a = Point x y where
    (x, y) = read $ fromPrimitivePersistValue p a

instance PersistField Point where
  persistName _ = "Point"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef [Left "point"]) False Nothing Nothing

-- These two instances of superclasses are useful but not necessary. They are like Functor and Applicative instances when you implement a Monad.
instance SinglePersistField Point where
  toSinglePersistValue = primToSinglePersistValue
  fromSinglePersistValue = primFromSinglePersistValue

instance PurePersistField Point where
  toPurePersistValues = primToPurePersistValues
  fromPurePersistValues = primFromPurePersistValues

data MobilePhone = MobilePhone {number :: String, prepaidMoney :: String, location :: Point, ipAddress :: String} deriving Show

mkPersist defaultCodegenConfig [groundhog|
- entity: MobilePhone
  constructors:
    - name: MobilePhone
      fields:
        - name: number
          type: varchar(13)
        - name: prepaidMoney
          type: money
# We don't need to put "typeName: point" for location because the dbType definition already has the required type
        - name: ipAddress
          type: inet
|]

main = withPostgresqlConn "dbname=test user=test password=test host=localhost" . runDbConn $ do
  let phone = MobilePhone "+1900 654 321" "100.456" (Point 4 6) "127.0.0.1"
  runMigration (migrate phone)
  k <- insert phone
  -- This will output the mobile phone data with money rounded to two fractional digits
  get k >>= liftIO . print
  liftIO $ putStrLn "This insert will make PostgreSQL throw an exception:"
  -- PGRES_FATAL_ERROR: ERROR:  value too long for type character varying(13)
  insert $ phone {number = "Phone number is too long now"}
