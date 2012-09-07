{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, OverloadedStrings #-}
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data User = User {name :: String, phoneNumber :: (String, String), bigAvatar :: ByteString} deriving (Eq, Show)

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: User
    keys:
      - name: unique_name
    constructors:
      - name: User
        uniques:
          - name: unique_name
            fields: [name]
|]

main :: IO ()
main = withSqliteConn ":memory:" $ runSqliteConn $ do
  let jack = User "Jack" ("+380", "12-345-67-89") "BMP"
      jill = User "Jill" ("+1", "98-765-43-12") "BMP"
  runMigration defaultMigrationLogger $ migrate jack
  mapM_ insert [jack, jill]
  -- get phones of the users. Only the required fields are fetched. Function project supports both regular and subfields
  phones <- project (PhoneNumberField ~> Tuple2_1Selector) $ (() ==. ()) `orderBy` [Asc AutoKeyField]
  liftIO $ print phones
  -- we can also use 'project' as a replacement for 'select' with extended options.
  -- the special datatype 'AutoKeyField' projects to the entity autokey, unique key phantoms project to keys, and the constructor phantoms project to the data itself
  withIds <- project (AutoKeyField, Unique_name, UserConstructor) (() ==. ())
  liftIO $ print withIds
