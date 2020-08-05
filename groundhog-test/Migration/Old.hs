{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Migration.Old where

import Database.Groundhog ()
import Database.Groundhog.TH

data AddColumn = AddColumn {addColumnOld :: Int} deriving (Eq, Show)

data AddUniqueConstraint = AddUniqueConstraint {addUniqueConstraint1 :: Int, addUniqueConstraint2 :: Int} deriving (Eq, Show)

data AddUniqueIndex = AddUniqueIndex {addUniqueIndex1 :: Int, addUniqueIndex2 :: Int} deriving (Eq, Show)

data AddConstructorToMany
  = AddConstructorToMany1 {addConstructorToMany1 :: Int}
  | AddConstructorToMany2 {addConstructorToMany2 :: String}
  deriving (Eq, Show)

data AddNotNull = AddNotNull {addNotNull :: Maybe String} deriving (Eq, Show)

data ChangeType = ChangeType {changeType :: Bool} deriving (Eq, Show)

mkPersist
  defaultCodegenConfig
  [groundhog|
- entity: AddColumn
- entity: AddUniqueConstraint
- entity: AddUniqueIndex
- entity: AddConstructorToMany
- entity: AddNotNull
- entity: ChangeType
|]
