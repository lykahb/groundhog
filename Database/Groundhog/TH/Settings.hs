{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings, RecordWildCards #-}

module Database.Groundhog.TH.Settings
  ( PersistSettings(..)
  , THEntityDef(..)
  , THEmbeddedDef(..)
  , THConstructorDef(..)
  , THFieldDef(..)
  , PSEntityDef(..)
  , PSEmbeddedDef(..)
  , PSConstructorDef(..)
  , PSFieldDef(..)
  , PSEmbeddedFieldDef(..)
  , PSConstraintDef(..)
  ) where

import Database.Groundhog.Generic (PSEmbeddedFieldDef(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Control.Applicative
import Control.Monad (mzero)
import Data.Yaml

data PersistSettings = PersistSettings {definitions :: [Either PSEntityDef PSEmbeddedDef]} deriving Show

-- data SomeData a = U1 { foo :: Int} | U2 { bar :: Maybe String, asc :: Int64, add :: a} | U3 deriving (Show, Eq)

data THEntityDef = THEntityDef {
    dataName :: Name -- SomeData
  , dbEntityName :: String  -- SQLSomeData
  , thTypeParams :: [TyVarBndr]
  , thConstructors :: [THConstructorDef]
} deriving Show

data THEmbeddedDef = THEmbeddedDef {
    embeddedName :: Name
  , embeddedConstructorName :: Name
  , dbEmbeddedName :: String -- used only to set polymorphic part of name of its container
  , thEmbeddedTypeParams :: [TyVarBndr]
  , embeddedFields :: [THFieldDef]
} deriving Show

data THConstructorDef = THConstructorDef {
    thConstrName    :: Name -- U2
  , thPhantomConstrName :: String -- U2Constructor
  , dbConstrName    :: String -- SQLU2
  , thConstrFields  :: [THFieldDef]
  , thConstrConstrs :: [PSConstraintDef]
} deriving Show

data THFieldDef = THFieldDef {
    fieldName :: String -- bar
  , dbFieldName :: String -- SQLbar
  , exprName :: String -- BarField
  , fieldType :: Type
  , embeddedDef :: Maybe [PSEmbeddedFieldDef]
} deriving Show

data PSEntityDef = PSEntityDef {
    psDataName :: String -- SomeData
  , psDbEntityName :: Maybe String  -- SQLSomeData
  , psConstructors :: Maybe [PSConstructorDef]
} deriving Show

data PSEmbeddedDef = PSEmbeddedDef {
    psEmbeddedName :: String
  , psDbEmbeddedName :: Maybe String -- used only to set polymorphic part of name of its container
  , psEmbeddedFields :: Maybe [PSFieldDef]
} deriving Show

data PSConstructorDef = PSConstructorDef {
    psConstrName    :: String -- U2
  , psPhantomConstrName :: Maybe String -- U2Constructor
  , psDbConstrName    :: Maybe String -- SQLU2
  , psConstrFields  :: Maybe [PSFieldDef]
  , psConstrConstrs :: Maybe [PSConstraintDef]
} deriving Show

data PSFieldDef = PSFieldDef {
    psFieldName :: String -- bar
  , psDbFieldName :: Maybe String -- SQLbar
  , psExprName :: Maybe String -- BarField
  , psEmbeddedDef :: Maybe [PSEmbeddedFieldDef]
} deriving Show

data PSConstraintDef = PSConstraintDef {
    psConstraintName :: String
  , psConstraintFields :: [String]
} deriving Show

instance Lift PersistSettings where
  lift (PersistSettings {..}) = [| PersistSettings $(lift definitions) |]

instance Lift PSEntityDef where
  lift (PSEntityDef {..}) = [| PSEntityDef $(lift psDataName) $(lift psDbEntityName) $(lift psConstructors) |]

instance Lift PSEmbeddedDef where
  lift (PSEmbeddedDef {..}) = [| PSEmbeddedDef $(lift psEmbeddedName) $(lift psDbEmbeddedName) $(lift psEmbeddedFields) |]

instance Lift PSConstructorDef where
  lift (PSConstructorDef {..}) = [| PSConstructorDef $(lift psConstrName) $(lift psPhantomConstrName) $(lift psDbConstrName) $(lift psConstrFields) $(lift psConstrConstrs) |]

instance Lift PSConstraintDef where
  lift (PSConstraintDef name fields) = [| PSConstraintDef $(lift name) $(lift fields) |]

instance Lift PSFieldDef where
  lift (PSFieldDef {..}) = [| PSFieldDef $(lift psFieldName) $(lift psDbFieldName) $(lift psExprName) $(lift psEmbeddedDef) |]

instance Lift PSEmbeddedFieldDef where
  lift (PSEmbeddedFieldDef {..}) = [| PSEmbeddedFieldDef $(lift psEmbeddedFieldName) $(lift psDbEmbeddedFieldName) $(lift psSubEmbedded) |]

instance FromJSON PersistSettings where
  {- it allows omitting parts of the settings file. All these forms are possible:
        definitions:
          - entity:name
        ---
          - entity:name
        ---
          entity: name
  -}
  parseJSON value = PersistSettings <$> case value of
    Object v -> do
      defs <- v .:? "definitions"
      case defs of
        Just defs'@(Array _) -> parseJSON defs'
        Just _ -> mzero
        Nothing -> fmap (\a -> [a]) $ parseJSON value
    defs@(Array _) -> parseJSON defs
    _ -> mzero

instance FromJSON (Either PSEntityDef PSEmbeddedDef) where
  parseJSON (Object v) = do
    entity   <- v .:? "entity"
    embedded <- v .:? "embedded"
    case (entity, embedded) of
      (Just _, Nothing) -> fmap Left $ PSEntityDef <$> v .: "entity" <*> v .:? "dbName" <*> v .:? "constructors"
      (Nothing, Just _) -> fmap Right $ PSEmbeddedDef <$> v .: "embedded" <*> v .:? "dbName" <*> v .:? "fields"
      (Just entName, Just embName) -> fail $ "Record has both entity name " ++ entName ++ " and embedded name " ++ embName
      (Nothing, Nothing) -> fail "Record must have either entity name or embedded name"
  parseJSON _          = mzero

instance FromJSON PSConstructorDef where
  parseJSON (Object v) = PSConstructorDef <$> v .: "name" <*> v .:? "phantomName" <*> v .:? "dbName" <*> v .:? "fields" <*> v .:? "constraints"
  parseJSON _          = mzero

instance FromJSON PSConstraintDef where
  parseJSON (Object v) = PSConstraintDef <$> v .: "name" <*> v .: "fields"
  parseJSON _          = mzero

instance FromJSON PSFieldDef where
  parseJSON (Object v) = PSFieldDef <$> v .: "name" <*> v .:? "dbName" <*> v .:? "exprName" <*> v .:? "embeddedType"
  parseJSON _          = mzero

instance FromJSON PSEmbeddedFieldDef where
  parseJSON (Object v) = PSEmbeddedFieldDef <$> v .: "name" <*> v .:? "dbName" <*> v .:? "embeddedType"
  parseJSON _          = mzero
