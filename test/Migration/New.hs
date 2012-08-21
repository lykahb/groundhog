{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances #-}
module Migration.New where
import Database.Groundhog
import Database.Groundhog.TH

data AddColumn = AddColumn {addColumnNew :: String, addColumnOld :: Int} deriving (Eq, Show)
data AddConstructorToMany = AddConstructorToMany0 {addConstructorToMany0 :: Int}
                          | AddConstructorToMany1 {addConstructorToMany1 :: Int}
                          | AddConstructorToMany2 {addConstructorToMany2 :: String} deriving (Eq, Show)

mkPersist suffixNamingStyle [groundhog|
- entity: AddColumn
- entity: AddConstructorToMany
|]
