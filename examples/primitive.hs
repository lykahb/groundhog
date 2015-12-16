{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleContexts, FlexibleInstances, StandaloneDeriving #-}
import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.TH
import Database.Groundhog.Core
import Database.Groundhog.Sqlite

data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Show, Enum)
data Time = Time {hour :: Int, minute :: Int}
  deriving (Eq, Show, Read)

data Alarm = Alarm {weekDay :: WeekDay, time :: Time}
  deriving (Eq, Show)

mkPersist defaultCodegenConfig [groundhog|
- entity: Alarm                  # Name of the datatype
- primitive: WeekDay             # For WeekDay PrimitivePersistField instance will be created
  converter: enumConverter       # It will be stored as an integer column. Conversion between WeekDay and PersistValue will use Enum instance.
- primitive: Time
  converter: showReadConverter   # It will be stored as a string column. Conversion between Time and PersistValue will use Show and Read instances.
|]

main = withSqliteConn ":memory:" $ runDbConn $ do
  runMigration $ do
    migrate (undefined :: Alarm)
  let alarm = Alarm Monday (Time 07 00)
  insert alarm
  alarms <- select $ WeekDayField ==. Monday
  liftIO $ print alarms