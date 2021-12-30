{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Groundhog.Test.Types.Types where

import qualified Control.Exception as E
import Data.Int
import Data.Word
import Database.Groundhog.Core (BackendSpecific, PersistEntity (..), Unique)
import Database.Groundhog.Instances ()
import Database.Groundhog.TH

data Number = Number {intDefault :: Int, int8 :: Int8, word8 :: Word8, int16 :: Int16, word16 :: Word16, int32 :: Int32, word32 :: Word32, int64 :: Int64, word64 :: Word64} deriving (Eq, Show)

data MaybeContext a = MaybeContext (Maybe a) deriving (Eq, Show)

data Single a = Single {single :: a} deriving (Eq, Show)

data Multi a = First {first :: Int} | Second {second :: a} deriving (Eq, Show)

data Settable = Settable {settable1 :: String, settable2 :: Maybe (Key (Single String) BackendSpecific), settableTuple :: (Int, (String, Maybe Int64))}

data Keys = Keys {refDirect :: Single String, refKey :: DefaultKey (Single String), refDirectMaybe :: Maybe (Single String), refKeyMaybe :: Maybe (DefaultKey (Single String))}

data EmbeddedSample = EmbeddedSample {embedded1 :: String, embedded2 :: (Int, Int)} deriving (Eq, Show)

data UniqueKeySample = UniqueKeySample {uniqueKey1 :: Int, uniqueKey2 :: Int, uniqueKey3 :: Maybe Int} deriving (Eq, Show)

data InCurrentSchema = InCurrentSchema {inCurrentSchema :: Maybe (Key InAnotherSchema BackendSpecific)}

data InAnotherSchema = InAnotherSchema {inAnotherSchema :: Maybe (Key InCurrentSchema BackendSpecific)}

data EnumTest = Enum1 | Enum2 | Enum3 deriving (Eq, Show, Enum)

data ShowRead = ShowRead String Int deriving (Eq, Show, Read)

newtype NotFieldInstance = NotFieldInstance String deriving (Eq, Show, Read)

data ConverterTest = ConverterTest {convertedField :: NotFieldInstance} deriving (Eq, Show, Read)

data NoColumns = NoColumns deriving (Eq, Show)

data NoKeys = NoKeys Int Int deriving (Eq, Show)

data ExpressionIndex = ExpressionIndex {expressionIndex :: Int} deriving (Eq, Show)

data TestException = TestException
  deriving (Show)

instance E.Exception TestException

notFieldInstanceConverter :: (NotFieldInstance -> String, String -> NotFieldInstance)
notFieldInstanceConverter = (\(NotFieldInstance s) -> s, NotFieldInstance)

mkPersist
  defaultCodegenConfig
  [groundhog|
- entity: Number
- entity: MaybeContext
- entity: Single
- entity: Multi
- entity: Keys
- entity: Settable
  dbName: sqlsettable
  constructors:
    - name: Settable
      phantomName: SettableFooBarConstructor
      dbName: entity_db_name_is_used_instead
      keyDbName: settable_id
      fields:
        - name: settable1
          dbName: sqlsettable1
          type: varchar(50)
          exprName: Settable1Fld
        - name: settable2
          reference:
            onDelete: cascade
        - name: settableTuple
          embeddedType:
            - name: val0
              dbName: firstTupleElement
            - name: val1
              embeddedType:
                - name: val0
                  dbName: secondTupleElement
                - name: val1
                  dbName: thirdTupleElement
                  reference:
                    table: sqlsettable
                    columns: [settable_id]
              dbName: name
      uniques:
        - name: someconstraint
          fields: [settable1, settable2]
- embedded: EmbeddedSample
  fields:
    - name: embedded2
      embeddedType:
        - name: val0
          dbName: embeddedTuple0
        - name: val1
          dbName: embeddedTuple1
- entity: UniqueKeySample
  autoKey: null
  keys:
    - name: unique_key_one_column
    - name: unique_key_two_columns
      default: true
  constructors:
    - name: UniqueKeySample
      uniques:
        - name: unique_key_one_column
          fields: [uniqueKey1]
        - name: unique_key_two_columns
          fields: [uniqueKey2, uniqueKey3]
        - name: unique_primary
          type: primary
          fields: [uniqueKey1, uniqueKey2]
- entity: InCurrentSchema
- entity: InAnotherSchema
  schema: myschema
- primitive: EnumTest
  converter: enumConverter
- primitive: ShowRead
  converter: showReadConverter
- entity: ConverterTest
  constructors:
    - name: ConverterTest
      fields:
        - name: convertedField
          converter: notFieldInstanceConverter
- entity: NoColumns
- entity: NoKeys
  autoKey: null
- entity: ExpressionIndex
  constructors:
    - name: ExpressionIndex
      uniques:
        - name: Expression_index
          type: index
          fields: [{expr: "(abs(\"expressionIndex\") + 1)" }]
|]

data HoldsUniqueKey = HoldsUniqueKey {foreignUniqueKey :: Key UniqueKeySample (Unique Unique_key_one_column)} deriving (Eq, Show)

mkPersist
  defaultCodegenConfig
  [groundhog|
- entity: HoldsUniqueKey
  keys:
    - name: foreignUniqueKey
  constructors:
  - name: HoldsUniqueKey
    uniques:
      - name: foreignUniqueKey
        fields: [foreignUniqueKey]
|]

-- cannot use ordinary deriving because it runs before mkPersist and requires (Single String) to be an instance of PersistEntity
deriving instance Eq Keys

deriving instance Show Keys

deriving instance Eq Settable

deriving instance Show Settable

deriving instance Eq InCurrentSchema

deriving instance Show InCurrentSchema

deriving instance Eq InAnotherSchema

deriving instance Show InAnotherSchema
