{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell #-}
module Migration.Old where
import Database.Groundhog.TH

data AddColumn = AddColumn {addColumnOld :: Int} deriving (Eq, Show)

deriveEntity ''AddColumn Nothing
