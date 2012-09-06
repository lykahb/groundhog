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

data Album  = Album  { albumName :: String} deriving (Eq, Show)
-- many-to-many relation
data ArtistAlbum = ArtistAlbum {artist :: Key Artist (Unique ArtistName), album :: Key Album BackendSpecific }
deriving instance Eq ArtistAlbum
deriving instance Show ArtistAlbum
-- We cannot use regular deriving because when it works, the Key Eq and Show instances for (Key Album BackendSpecific) are not created yet
data Track  = Track  { albumTrack :: Key Album BackendSpecific, trackName :: String }
deriving instance Eq Track
deriving instance Show Track

mkPersist suffixNamingStyle [groundhog|
definitions:
  - entity: Album
  - entity: Track
  # keys of many-to-many relation form a unique key
  - entity: ArtistAlbum
    autoKey: null
    keys:
      - name: ArtistAlbumKey
        default: true
    constructors:
      - name: ArtistAlbum
        uniques:
          - name: ArtistAlbumKey
            fields: [artist, album]
|]

main :: IO ()
main = withSqliteConn ":memory:" $ runSqliteConn $ do
  let artists = [Artist "John Lennon", Artist "George Harrison"]
      imagineAlbum = Album "Imagine"
  runMigration defaultMigrationLogger $ do
    migrate (undefined :: ArtistAlbum)
    migrate (undefined :: Track)
  mapM_ insert artists

  imagineKey <- insert imagineAlbum
  let tracks = map (Track imagineKey) ["Imagine", "Crippled Inside", "Jealous Guy", "It's So Hard", "I Don't Want to Be a Soldier, Mama, I Don't Want to Die", "Gimme Some Truth", "Oh My Love", "How Do You Sleep?", "How?", "Oh Yoko!"]
  mapM_ insert tracks
  mapM_ (\artist -> insert $ ArtistAlbum (extractUnique artist) imagineKey) artists
  -- print first 3 tracks from any album with John Lennon
  [albumKey'] <- project AlbumField (ArtistField ==. ArtistNameKey "John Lennon") [] 1 0
  -- order by primary key
  tracks' <- select (AlbumTrackField ==. albumKey') [Asc AutoKeyField] 3 0
  liftIO $ print tracks'

