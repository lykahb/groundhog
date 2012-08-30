{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances #-}
module Migration.Old where
import Database.Groundhog
import Database.Groundhog.TH

data AddColumn = AddColumn {addColumnOld :: Int} deriving (Eq, Show)
data AddUnique = AddUnique {column1 :: Int, column2 :: String} deriving (Eq, Show)
data AddConstructorToMany = AddConstructorToMany1 {addConstructorToMany1 :: Int}
                          | AddConstructorToMany2 {addConstructorToMany2 :: String} deriving (Eq, Show)

mkPersist suffixNamingStyle [groundhog|
- entity: AddColumn
- entity: AddUnique
- entity: AddConstructorToMany
|]
