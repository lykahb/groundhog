{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Groundhog.TH.Settings
  ( PersistDefinitions(..)
  , PersistDefinition(..)
  , THEntityDef(..)
  , THEmbeddedDef(..)
  , THPrimitiveDef(..)
  , THConstructorDef(..)
  , THFieldDef(..)
  , THUniqueDef(..)
  , THUniqueKeyDef(..)
  , THAutoKeyDef(..)
  , PSEntityDef(..)
  , PSEmbeddedDef(..)
  , PSPrimitiveDef(..)
  , PSConstructorDef(..)
  , PSFieldDef(..)
  , PSUniqueDef(..)
  , PSUniqueKeyDef(..)
  , PSAutoKeyDef(..)
  ) where

import Database.Groundhog.Core (UniqueType(..), ReferenceActionType(..))
import Database.Groundhog.Generic (PSFieldDef(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Control.Applicative
import Control.Monad (mzero)
import Data.Yaml

data PersistDefinitions = PersistDefinitions {definitions :: [PersistDefinition]} deriving Show
data PersistDefinition = PSEntityDef' PSEntityDef
                       | PSEmbeddedDef' PSEmbeddedDef
                       | PSPrimitiveDef' PSPrimitiveDef
     deriving Show

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
  , thDbEmbeddedName :: String -- ^ It is used only to set polymorphic part of name of its container
  , thEmbeddedTypeParams :: [TyVarBndr]
  , thEmbeddedFields :: [THFieldDef]
} deriving Show

data THPrimitiveDef = THPrimitiveDef {
    thPrimitiveName :: Name
  , thPrimitiveDbName :: String -- ^ It is used only to set polymorphic part of name of its container
  , thPrimitiveStringEnumRepresentation :: Bool -- ^ Store in database as string using Show/Read instances (True) or as integer using Enum instance (False).
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
  , thEmbeddedDef :: Maybe [PSFieldDef String]
  , thDefaultValue :: Maybe String
  , thReferenceParent :: Maybe (Maybe (Maybe String, String, [String]), Maybe ReferenceActionType, Maybe ReferenceActionType)
} deriving Show

data THUniqueDef = THUniqueDef {
    thUniqueName :: String
  , thUniqueType :: UniqueType
  , thUniqueFields :: [String] -- ^ Names of fields, not column names, i.e, thFieldName
} deriving Show

data THUniqueKeyDef = THUniqueKeyDef {
    thUniqueKeyName :: String
  , thUniqueKeyPhantomName :: String
  , thUniqueKeyConstrName :: String
  , thUniqueKeyDbName :: String -- ^ It is used only to set polymorphic part of name of its container
    -- | It should repeat fields from THUniqueDef but it may give different settings for them. It is done to allow foreign key fields to be different from parent fields of the entity. These fields are used for creating a the key constructor and instances for it. For example, it can have a default value, or even a different type (INT8 may reference INT4).
  , thUniqueKeyFields :: [THFieldDef]
  , thUniqueKeyMakeEmbedded :: Bool -- ^ If True, make it an instance of Embedded
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
  , psDbEmbeddedName :: Maybe String -- ^ It is used only to set polymorphic part of name of its container
  , psEmbeddedFields :: Maybe [PSFieldDef String]
} deriving Show

data PSPrimitiveDef = PSPrimitiveDef {
    psPrimitiveName :: String
  , psPrimitiveDbName :: Maybe String -- ^ It is used only to set polymorphic part of name of its container
  , psPrimitiveStringEnumRepresentation :: Maybe Bool -- ^ Store in database as string using Show/Read instances (True) or as integer using Enum instance (False).
} deriving Show

data PSConstructorDef = PSConstructorDef {
    psConstrName :: String -- U2
  , psPhantomConstrName :: Maybe String -- U2Constructor
  , psDbConstrName :: Maybe String -- SQLU2
  , psDbAutoKeyName :: Maybe (Maybe String) -- u2_id
  , psConstrFields  :: Maybe [PSFieldDef String]
  , psConstrUniques :: Maybe [PSUniqueDef]
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
  , psUniqueKeyFields :: Maybe [PSFieldDef String]
  , psUniqueKeyMakeEmbedded :: Maybe Bool
  , psUniqueKeyIsDef :: Maybe Bool
} deriving Show

data PSAutoKeyDef = PSAutoKeyDef {
    psAutoKeyConstrName :: Maybe String
  , psAutoKeyIsDef :: Maybe Bool
} deriving Show

instance Lift PersistDefinition where
  lift (PSEntityDef' e) = [| PSEntityDef' e |]
  lift (PSEmbeddedDef' e) = [| PSEmbeddedDef' e |]
  lift (PSPrimitiveDef' e) = [| PSPrimitiveDef' e |]

instance Lift PSPrimitiveDef where
  lift (PSPrimitiveDef {..}) = [| PSPrimitiveDef $(lift psPrimitiveName) $(lift psPrimitiveDbName) $(lift psPrimitiveStringEnumRepresentation) |]

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
  lift (UniquePrimary x) = [| UniquePrimary $(lift x) |]

instance Lift ReferenceActionType where
  lift NoAction = [| NoAction |]
  lift Restrict = [| Restrict |]
  lift Cascade = [| Cascade |]
  lift SetNull = [| SetNull |]
  lift SetDefault = [| SetDefault |]

instance Lift (PSFieldDef String) where
  lift (PSFieldDef {..}) = [| PSFieldDef $(lift psFieldName) $(lift psDbFieldName) $(lift psDbTypeName) $(lift psExprName) $(lift psEmbeddedDef) $(lift psDefaultValue) $(lift psReferenceParent) |]

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

instance FromJSON PersistDefinition where
  parseJSON obj = PSEntityDef' <$> parseJSON obj
              <|> PSEmbeddedDef' <$> parseJSON obj
              <|> PSPrimitiveDef' <$> parseJSON obj

instance FromJSON PSEntityDef where
  parseJSON (Object v) = PSEntityDef <$> v .: "entity" <*> v .:? "dbName" <*> v .:? "schema" <*> optional (v .: "autoKey") <*> v .:? "keys" <*> v .:? "constructors"
  parseJSON _          = mzero

instance FromJSON PSEmbeddedDef where
  parseJSON (Object v) = PSEmbeddedDef <$> v .: "embedded" <*> v .:? "dbName" <*> v .:? "fields"
  parseJSON _          = mzero

instance FromJSON PSPrimitiveDef where
  parseJSON (Object v) = do
    x <- v .:? "representation"
    let representation = case x of
          Nothing -> pure True
          Just "showread" -> pure True
          Just "enum" -> pure False
          Just r -> fail $ "parseJSON: representation expected [\"showread\",\"enum\"], but got " ++ r
    PSPrimitiveDef <$> v .: "primitive" <*> v .:? "dbName" <*> pure representation
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
    let vals = [("constraint", UniqueConstraint), ("index", UniqueIndex), ("primary", UniquePrimary False)]
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

instance FromJSON (PSFieldDef String) where
  parseJSON (Object v) = PSFieldDef <$> v .: "name" <*> v .:? "dbName" <*> v .:? "type" <*> v .:? "exprName" <*> v .:? "embeddedType" <*> v .:? "default" <*> mkRefSettings where
    mkRefSettings = do
      ref <- v .:? "reference"
      (parent, onDel, onUpd) <- case ref of
        Just (Object r) -> (,,) <$> parentRef <*> r .:? "onDelete" <*> r .:? "onUpdate" where
          parentRef = optional ((,,) <$> r .:? "schema" <*> r .: "table" <*> r .: "columns")
        _ -> pure (Nothing, Nothing, Nothing)
      -- this temporary solution uses onDelete and onUpdate both from inside reference object (preferred) and from field level (for compatibility)
      (onDel', onUpd') <- (,) <$> v .:? "onDelete" <*> v .:? "onUpdate"
      pure $ case (parent, onDel <|> onDel', onUpd <|> onUpd') of
        (Nothing, Nothing, Nothing) -> Nothing
        refSettings -> Just refSettings
  parseJSON _          = mzero

instance FromJSON PSUniqueKeyDef where
  parseJSON (Object v) = PSUniqueKeyDef <$> v .: "name" <*> v .:? "keyPhantom" <*> v .:? "constrName" <*> v .:? "dbName" <*> v .:? "fields" <*> v .:? "mkEmbedded" <*> v .:? "default"
  parseJSON _          = mzero

instance FromJSON PSAutoKeyDef where
  parseJSON (Object v) = PSAutoKeyDef <$> v .:? "constrName" <*> v .:? "default"
  parseJSON _          = mzero
