{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data Artist = Artist { artistName :: String } deriving (Eq, Show)
data Album  = Album  { albumName :: String} deriving (Eq, Show)
-- We cannot use regular deriving because when it works, the Key Eq and Show instances for (Key Album BackendSpecific) are not created yet
data Track  = Track  { albumTrack :: Key Album BackendSpecific, trackName :: String }
deriving instance Eq Track
deriving instance Show Track

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: Artist
    autoKey:
      constrName: AutoKey
      default: false # Defines if this key is used when an entity is stored directly, for example, data Ref = Ref Artist
    keys:
      - name: ArtistName
        default: true
    constructors:
      - name: Artist
        uniques:
          - name: ArtistName
            # Optional parameter type can be constraint (by default), index, or primary
            type: constraint
            fields: [artistName]
  - entity: Album
  - entity: Track
    constructors:
      - name: Track
        fields:
          - name: albumTrack
  # Configure actions on parent table changes
            onDelete: cascade
            onUpdate: restrict
|]

-- Many-to-many relation. It is defined here because ArtistName is available only after the the definitions for Artist are created
data ArtistAlbum = ArtistAlbum {artist :: Key Artist (Unique ArtistName), album :: Key Album BackendSpecific }
deriving instance Eq ArtistAlbum
deriving instance Show ArtistAlbum

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: ArtistAlbum
    autoKey: null # Disable creation of the autoincrement integer key
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
main = withSqliteConn ":memory:" $ runDbConn $ do
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
  [albumKey'] <- project AlbumField $ (ArtistField ==. ArtistNameKey "John Lennon") `limitTo` 1
  -- order by primary key
  tracks' <- select $ (AlbumTrackField ==. albumKey') `orderBy` [Desc AutoKeyField] `limitTo` 3
  liftIO $ print tracks'
