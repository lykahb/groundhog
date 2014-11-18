{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
module Datatypes where
import Database.Groundhog
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Int (Int32, Int64)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Time.LocalTime (ZonedTime)
data Tbl1
    = Tbl1 {tbl1A :: (Maybe Int32),
            tbl1B :: (Maybe Int),
            tbl1C :: (Maybe String),
            tbl1D :: (Maybe Double),
            tbl1E :: (Maybe ByteString),
            tbl1F :: (Maybe Day),
            tbl1G :: (Maybe TimeOfDay),
            tbl1H :: UTCTime,
            tbl1J :: (Maybe ByteString)}
data Tbl2
    = Tbl2 {tbl2A :: (Maybe Day),
            tbl2B :: (Maybe Day),
            tbl2C :: (Maybe ByteString),
            tbl2D :: (Maybe ByteString),
            tbl2E :: (Maybe ByteString)}
