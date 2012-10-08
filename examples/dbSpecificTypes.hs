{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances #-}
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (unpack)
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.TH
import Database.Groundhog.Postgresql

data Point = Point { pointX :: Int, pointY :: Int } deriving Show

-- PostgreSQL keeps point in format "(x,y)"
instance PrimitivePersistField Point where
  toPrimitivePersistValue _ (Point x y) = PersistString $ "(" ++ show x ++ "," ++ show y ++ ")"
  -- Crude parsing. Rely on the correspondence between Haskell tuple format and PostgreSQL point format.
  fromPrimitivePersistValue _ (PersistString a) = let (x, y) = read a in Point x y
  fromPrimitivePersistValue _ (PersistByteString a) = let (x, y) = read (unpack a) in Point x y

instance PersistField Point where
  persistName _ = "Point"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbOther "point"

data MobilePhone = MobilePhone {number :: String, prepaidMoney :: String, location :: Point, ipAddress :: String} deriving Show

-- Code generator will derive the necessary instances and generate other boilerplate code for the entities defined below.
-- It will also create function migrateAll that migrates schema for all non-polymorphic entities 
mkPersist defaultCodegenConfig [groundhog|
- entity: MobilePhone
  constructors:
    - name: MobilePhone
      fields:
        - name: number
          typeName: varchar(13)
        - name: prepaidMoney
          typeName: money
# We don't need to put "typeName: point" for location because the dbType definition already has the required type
        - name: ipAddress
          typeName: inet
|]

main = withPostgresqlConn "dbname=test user=test password=test host=localhost" . runPostgresqlConn $ do
  let phone = MobilePhone "+1900 654 321" "100.456" (Point 4 6) "127.0.0.1"
  runMigration defaultMigrationLogger (migrate phone)
  k <- insert phone
  -- This will output the mobile phone data with money rounded to two fractional digits
  get k >>= liftIO . print
  liftIO $ putStrLn "This insert will make PostgreSQL throw an exception:"
  -- PGRES_FATAL_ERROR: ERROR:  value too long for type character varying(13)\n"}
  insert $ phone {number = "Phone number is too long now"}
