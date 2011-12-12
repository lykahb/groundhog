{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell #-}
module Migration.New where
import Database.Groundhog.TH

data AddColumn = AddColumn {addColumnNew :: String, addColumnOld :: Int} deriving (Eq, Show)

deriveEntity ''AddColumn Nothing
