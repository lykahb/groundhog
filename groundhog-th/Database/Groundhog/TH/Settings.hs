{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Groundhog.TH.Settings
  ( PersistDefinitions(..)
  , THEntityDef(..)
  , THEmbeddedDef(..)
  , THConstructorDef(..)
  , THFieldDef(..)
  , THUniqueDef(..)
  , THUniqueKeyDef(..)
  , THAutoKeyDef(..)
  , PSEntityDef(..)
  , PSEmbeddedDef(..)
  , PSConstructorDef(..)
  , PSFieldDef(..)
  , PSEmbeddedFieldDef(..)
  , PSUniqueDef(..)
  , PSUniqueKeyDef(..)
  , PSAutoKeyDef(..)
  ) where

import Database.Groundhog.Core (UniqueType(..), ReferenceActionType(..))
import Database.Groundhog.Generic (PSEmbeddedFieldDef(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Control.Applicative
import Control.Monad (mzero)
import Data.Yaml

data PersistDefinitions = PersistDefinitions {definitions :: [Either PSEntityDef PSEmbeddedDef]} deriving Show

-- data SomeData a = U1 { foo :: Int} | U2 { bar :: Maybe String, asc :: Int64, add :: a} | U3 deriving (Show, Eq)

data THEntityDef = THEntityDef {
    thDataName :: Name -- SomeData
  , thDbEntityName :: String  -- SQLSomeData
  , thEntitySchema :: Maybe String
  , thAutoKey :: Maybe THAutoKeyDef
  , thUniqueKeys :: [THUniqueKeyDef]
  , thTypeParams :: [TyVarBndr]
  , thConstructors :: [THConstructorDef]
} deriving Show

data THAutoKeyDef = THAutoKeyDef {
    thAutoKeyConstrName :: String
  , thAutoKeyIsDef :: Bool
} deriving Show

data THEmbeddedDef = THEmbeddedDef {
    thEmbeddedName :: Name
  , thEmbeddedConstructorName :: Name
  , thDbEmbeddedName :: String -- used only to set polymorphic part of name of its container
  , thEmbeddedTypeParams :: [TyVarBndr]
  , thEmbeddedFields :: [THFieldDef]
} deriving Show

data THConstructorDef = THConstructorDef {
    thConstrName    :: Name -- U2
  , thPhantomConstrName :: String -- U2Constructor
  , thDbConstrName    :: String -- SQLU2
  , thDbAutoKeyName :: Maybe String -- u2_id
  , thConstrFields  :: [THFieldDef]
  , thConstrUniques :: [THUniqueDef]
} deriving Show

data THFieldDef = THFieldDef {
    thFieldName :: String -- bar
  , thDbFieldName :: String -- SQLbar
  , thDbTypeName :: Maybe String -- inet, NUMERIC(5, 2), VARCHAR(50)
  , thExprName :: String -- BarField
  , thFieldType :: Type
  , thEmbeddedDef :: Maybe [PSEmbeddedFieldDef]
  , thFieldOnDelete :: Maybe ReferenceActionType
  , thFieldOnUpdate :: Maybe ReferenceActionType
} deriving Show

data THUniqueDef = THUniqueDef {
    thUniqueName :: String
  , thUniqueType :: UniqueType
  , thUniqueFields :: [String]
} deriving Show

data THUniqueKeyDef = THUniqueKeyDef {
    thUniqueKeyName :: String
  , thUniqueKeyPhantomName :: String
  , thUniqueKeyConstrName :: String
  , thUniqueKeyDbName :: String -- used only to set polymorphic part of name of its container
  , thUniqueKeyFields :: [THFieldDef]
  , thUniqueKeyMakeEmbedded :: Bool -- whether to make it an instance of Embedded
  , thUniqueKeyIsDef :: Bool
} deriving Show

data PSEntityDef = PSEntityDef {
    psDataName :: String -- SomeData
  , psDbEntityName :: Maybe String  -- SQLSomeData
  , psEntitySchema :: Maybe String
  , psAutoKey :: Maybe (Maybe PSAutoKeyDef) -- SomeDataKey. Nothing - default key. Just Nothing - no autokey. Just (Just _) - specify autokey settings
  , psUniqueKeys :: Maybe [PSUniqueKeyDef]
  , psConstructors :: Maybe [PSConstructorDef]
} deriving Show

data PSEmbeddedDef = PSEmbeddedDef {
    psEmbeddedName :: String
  , psDbEmbeddedName :: Maybe String -- used only to set polymorphic part of name of its container
  , psEmbeddedFields :: Maybe [PSFieldDef]
} deriving Show

data PSConstructorDef = PSConstructorDef {
    psConstrName :: String -- U2
  , psPhantomConstrName :: Maybe String -- U2Constructor
  , psDbConstrName :: Maybe String -- SQLU2
  , psDbAutoKeyName :: Maybe (Maybe String) -- u2_id
  , psConstrFields  :: Maybe [PSFieldDef]
  , psConstrUniques :: Maybe [PSUniqueDef]
} deriving Show

data PSFieldDef = PSFieldDef {
    psFieldName :: String -- bar
  , psDbFieldName :: Maybe String -- SQLbar
  , psDbTypeName :: Maybe String -- inet, NUMERIC(5,2), VARCHAR(50)
  , psExprName :: Maybe String -- BarField
  , psEmbeddedDef :: Maybe [PSEmbeddedFieldDef]
  , psFieldOnDelete :: Maybe ReferenceActionType
  , psFieldOnUpdate :: Maybe ReferenceActionType
} deriving Show

data PSUniqueDef = PSUniqueDef {
    psUniqueName :: String
  , psUniqueType :: Maybe UniqueType
  , psUniqueFields :: [String]
} deriving Show

data PSUniqueKeyDef = PSUniqueKeyDef {
    psUniqueKeyName :: String
  , psUniqueKeyPhantomName :: Maybe String
  , psUniqueKeyConstrName :: Maybe String
  , psUniqueKeyDbName :: Maybe String
  , psUniqueKeyFields :: Maybe [PSFieldDef]
  , psUniqueKeyMakeEmbedded :: Maybe Bool
  , psUniqueKeyIsDef :: Maybe Bool
} deriving Show

data PSAutoKeyDef = PSAutoKeyDef {
    psAutoKeyConstrName :: Maybe String
  , psAutoKeyIsDef :: Maybe Bool
} deriving Show

instance Lift PersistDefinitions where
  lift (PersistDefinitions {..}) = [| PersistDefinitions $(lift definitions) |]

instance Lift PSEntityDef where
  lift (PSEntityDef {..}) = [| PSEntityDef $(lift psDataName) $(lift psDbEntityName) $(lift psEntitySchema) $(lift psAutoKey) $(lift psUniqueKeys) $(lift psConstructors) |]

instance Lift PSEmbeddedDef where
  lift (PSEmbeddedDef {..}) = [| PSEmbeddedDef $(lift psEmbeddedName) $(lift psDbEmbeddedName) $(lift psEmbeddedFields) |]

instance Lift PSConstructorDef where
  lift (PSConstructorDef {..}) = [| PSConstructorDef $(lift psConstrName) $(lift psPhantomConstrName) $(lift psDbConstrName) $(lift psDbAutoKeyName) $(lift psConstrFields) $(lift psConstrUniques) |]

instance Lift PSUniqueDef where
  lift (PSUniqueDef name typ fields) = [| PSUniqueDef $(lift name) $(lift typ) $(lift fields) |]

instance Lift UniqueType where
  lift UniqueConstraint = [| UniqueConstraint |]
  lift UniqueIndex = [| UniqueIndex |]
  lift UniquePrimary = [| UniquePrimary |]

instance Lift ReferenceActionType where
  lift NoAction = [| NoAction |]
  lift Restrict = [| Restrict |]
  lift Cascade = [| Cascade |]
  lift SetNull = [| SetNull |]
  lift SetDefault = [| SetDefault |]

instance Lift PSFieldDef where
  lift (PSFieldDef {..}) = [| PSFieldDef $(lift psFieldName) $(lift psDbFieldName) $(lift psDbTypeName) $(lift psExprName) $(lift psEmbeddedDef) $(lift psFieldOnDelete) $(lift psFieldOnUpdate) |]

instance Lift PSEmbeddedFieldDef where
  lift (PSEmbeddedFieldDef {..}) = [| PSEmbeddedFieldDef $(lift psEmbeddedFieldName) $(lift psDbEmbeddedFieldName) $(lift psDbEmbeddedTypeName) $(lift psSubEmbedded) |]

instance Lift PSUniqueKeyDef where
  lift (PSUniqueKeyDef {..}) = [| PSUniqueKeyDef $(lift psUniqueKeyName) $(lift psUniqueKeyPhantomName) $(lift psUniqueKeyConstrName) $(lift psUniqueKeyDbName) $(lift psUniqueKeyFields) $(lift psUniqueKeyMakeEmbedded) $(lift psUniqueKeyIsDef) |]

instance Lift PSAutoKeyDef where
  lift (PSAutoKeyDef {..}) = [| PSAutoKeyDef $(lift psAutoKeyConstrName) $(lift psAutoKeyIsDef) |]

instance FromJSON PersistDefinitions where
  {- it allows omitting parts of the settings file. All these forms are possible:
        definitions:
          - entity:name
        ---
          - entity:name
        ---
          entity: name
  -}
  parseJSON value = PersistDefinitions <$> case value of
    Object v -> do
      defs <- v .:? "definitions"
      case defs of
        Just defs'@(Array _) -> parseJSON defs'
        Just _ -> mzero
        Nothing -> fmap (\a -> [a]) $ parseJSON value
    defs@(Array _) -> parseJSON defs
    _ -> mzero

instance FromJSON (Either PSEntityDef PSEmbeddedDef) where
  parseJSON obj@(Object v) = do
    entity   <- v .:? "entity"
    embedded <- v .:? "embedded"
    case (entity, embedded) of
      (Just _, Nothing) -> fmap Left $ parseJSON obj
      (Nothing, Just _) -> fmap Right $ parseJSON obj
      (Just entName, Just embName) -> fail $ "Record has both entity name " ++ entName ++ " and embedded name " ++ embName
      (Nothing, Nothing) -> fail "Record must have either entity name or embedded name"
  parseJSON _          = mzero

instance FromJSON PSEntityDef where
  parseJSON (Object v) = PSEntityDef <$> v .: "entity" <*> v .:? "dbName" <*> v .:? "schema" <*> optional (v .: "autoKey") <*> v .:? "keys" <*> v .:? "constructors"
  parseJSON _          = mzero

instance FromJSON PSEmbeddedDef where
  parseJSON (Object v) = PSEmbeddedDef <$> v .: "embedded" <*> v .:? "dbName" <*> v .:? "fields"
  parseJSON _          = mzero

instance FromJSON PSConstructorDef where
  parseJSON (Object v) = PSConstructorDef <$> v .: "name" <*> v .:? "phantomName" <*> v .:? "dbName" <*> v .:? "keyDbName" <*> v .:? "fields" <*> v .:? "uniques"
  parseJSON _          = mzero

instance FromJSON PSUniqueDef where
  parseJSON (Object v) = PSUniqueDef <$> v .: "name" <*> v .:? "type" <*> v .: "fields"
  parseJSON _          = mzero

instance FromJSON UniqueType where
  parseJSON o = do
    x <- parseJSON o
    let vals = [("constraint", UniqueConstraint), ("index", UniqueIndex), ("primary", UniquePrimary)]
    case lookup x vals of
      Just a -> return a
      Nothing -> fail $ "parseJSON: UniqueType expected " ++ show (map fst vals) ++ ", but got " ++ x

instance FromJSON ReferenceActionType where
  parseJSON o = do
    x <- parseJSON o
    let vals = [("no action", NoAction), ("restrict", Restrict), ("cascade", Cascade), ("set null", SetNull), ("set default", SetDefault)]
    case lookup x vals of
      Just a -> return a
      Nothing -> fail $ "parseJSON: UniqueType expected " ++ show (map fst vals) ++ ", but got " ++ x

instance FromJSON PSFieldDef where
  parseJSON (Object v) = PSFieldDef <$> v .: "name" <*> v .:? "dbName" <*> v .:? "type" <*> v .:? "exprName" <*> v .:? "embeddedType" <*> v .:? "onDelete" <*> v .:? "onUpdate"
  parseJSON _          = mzero

instance FromJSON PSEmbeddedFieldDef where
  parseJSON (Object v) = PSEmbeddedFieldDef <$> v .: "name" <*> v .:? "dbName" <*> v .:? "type" <*> v .:? "embeddedType"
  parseJSON _          = mzero

instance FromJSON PSUniqueKeyDef where
  parseJSON (Object v) = PSUniqueKeyDef <$> v .: "name" <*> v .:? "keyPhantom" <*> v .:? "constrName" <*> v .:? "dbName" <*> v .:? "fields" <*> v .:? "mkEmbedded" <*> v .:? "default"
  parseJSON _          = mzero

instance FromJSON PSAutoKeyDef where
  parseJSON (Object v) = PSAutoKeyDef <$> v .:? "constrName" <*> v .:? "default"
  parseJSON _          = mzero
