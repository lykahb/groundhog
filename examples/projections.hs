{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances #-}
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data User = User {name :: String, phoneNumber :: (String, String), bigAvatar :: BS.ByteString} deriving (Eq, Show)

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
  let jack = User "Jack" ("+380", "12-345-67-89") (BS.pack "BMP")
      jill = User "Jill" ("+1", "98-765-43-12") (BS.pack "BMP")
  runMigration defaultMigrationLogger $ migrate jack
  mapM_ insert [jack, jill]
  -- get usernames and phones. Only the required fields are fetched (phone in this case). Function project supports both regular and subfields. The expressions may have complex structure which includes SQL operators and functions
  liftIO $ putStrLn "Uppercase usernames and phones"
  phones <- project (upper ("username: " `append` NameField), PhoneNumberField ~> Tuple2_1Selector) $ (lower NameField `like` "ja%") `orderBy` [Asc AutoKeyField]
  liftIO $ print phones
  -- we can also use 'project' as a replacement for 'select' with extended options.
  liftIO $ putStrLn "The special datatype 'AutoKeyField' projects to the entity autokey, unique key phantoms project to keys, and the constructor phantoms project to the data itself"
  withIds <- project (AutoKeyField, Unique_name, UserConstructor) (() ==. ())
  liftIO $ print withIds
