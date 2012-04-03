{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes #-}
module Migration.Old where
import Database.Groundhog
import Database.Groundhog.TH

data AddColumn = AddColumn {addColumnOld :: Int} deriving (Eq, Show)
data AddConstructorToMany = AddConstructorToMany1 {addConstructorToMany1 :: Int}
                          | AddConstructorToMany2 {addConstructorToMany2 :: String} deriving (Eq, Show)

mkPersist fieldNamingStyle [groundhog|
- entity: AddColumn
- entity: AddConstructorToMany
|]