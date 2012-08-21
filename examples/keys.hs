{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
import Control.Monad
import Control.Monad.IO.Class(liftIO)
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

-- artistName is a unique key
data Artist = Artist { artistName :: String } deriving (Eq, Show)

mkPersist suffixNamingStyle [groundhog|
definitions:
  - entity: Artist
    autoKey:
      constrName: AutoKey
      default: false # Defines if this key is used when an entity is stored directly, for example, data Ref = Ref SomeEntity
    keys:
      - name: ArtistName
        default: true
    constructors:
      - name: Artist
        uniques:
          - name: ArtistName
            fields: [artistName]
|]

data Album  = Album  { albumName :: String, artist :: Artist, collaborator :: Maybe (Key Artist (Unique ArtistName)) } deriving (Eq, Show)
-- We cannot use regular deriving because when it works, the Key Eq and Show instances are not created yet
data Track  = Track  { trackName :: String, album :: Key Album BackendSpecific }
deriving instance Eq Track
deriving instance Show Track

mkPersist suffixNamingStyle [groundhog|
definitions:
  - entity: Album
  - entity: Track
|]

main :: IO ()
main = withSqliteConn ":memory:" $ runSqliteConn $ do
  let john = Artist "John Lennon"
      george = Artist "George Harrison"
      imagineAlbum = Album "Imagine" john (Just $ extractUnique george)
  runMigration defaultMigrationLogger $ migrate (undefined :: Track)
  insert george
  albumKey <- insert imagineAlbum
  let track = Track "Imagine" albumKey
  insert track
  liftIO $ print track
  return ()
  {-
  
  runMigration defaultMigrationLogger $ migrate john
  mapM_ insert [john, jane]
  -- get phones of the users. Only the required fields are fetched. Function project supports both regular and subfields
  phones <- project (PhoneNumberField ~> Tuple2_1Selector) (() ==. ()) [Asc AutoKeyField] 0 0
  liftIO $ print phones
  -- we can also use 'project' as a replacement for 'select' with extended options.
  -- the special datatype 'AutoKeyField' projects to the entity autokey, unique key phantoms project to keys, and the constructor phantoms project to the data itself
  withIds <- project (AutoKeyField, Unique_name, UserConstructor) (() ==. ()) [] 0 0
  liftIO $ print withIds
-}
