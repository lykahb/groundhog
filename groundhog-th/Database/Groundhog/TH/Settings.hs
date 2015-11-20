{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Groundhog.TH.Settings
  ( PersistDefinitions(..)
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
import Control.Monad (forM, mzero)
import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Foldable as Fold
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes)
import Data.Text (Text)

data PersistDefinitions = PersistDefinitions {psEntities :: [PSEntityDef], psEmbeddeds :: [PSEmbeddedDef], psPrimitives :: [PSPrimitiveDef]} deriving Show

-- data SomeData a = U1 { foo :: Int} | U2 { bar :: Maybe String, asc :: Int64, add :: a} | U3 deriving (Show, Eq)

data THEntityDef = THEntityDef {
    thDataName :: Name -- SomeData
  , thDbEntityName :: String  -- SQLSomeData
  , thEntitySchema :: Maybe String
  , thAutoKey :: Maybe THAutoKeyDef
  , thUniqueKeys :: [THUniqueKeyDef]
  , thTypeParams :: [TyVarBndr]
  , thConstructors :: [THConstructorDef]
} deriving (Eq, Show)

data THAutoKeyDef = THAutoKeyDef {
    thAutoKeyConstrName :: String
  , thAutoKeyIsDef :: Bool
} deriving (Eq, Show)

data THEmbeddedDef = THEmbeddedDef {
    thEmbeddedName :: Name
  , thEmbeddedConstructorName :: Name
  , thDbEmbeddedName :: String -- ^ It is used only to set polymorphic part of name of its container
  , thEmbeddedTypeParams :: [TyVarBndr]
  , thEmbeddedFields :: [THFieldDef]
} deriving (Eq, Show)

data THPrimitiveDef = THPrimitiveDef {
    thPrimitiveName :: Name
  , thPrimitiveDbName :: String -- ^ It is used only to set polymorphic part of name of its container
  , thPrimitiveStringEnumRepresentation :: Bool -- ^ Store in database as string using Show/Read instances (True) or as integer using Enum instance (False).
} deriving (Eq, Show)

data THConstructorDef = THConstructorDef {
    thConstrName    :: Name -- U2
  , thPhantomConstrName :: String -- U2Constructor
  , thDbConstrName    :: String -- SQLU2
  , thDbAutoKeyName :: Maybe String -- u2_id
  , thConstrFields  :: [THFieldDef]
  , thConstrUniques :: [THUniqueDef]
} deriving (Eq, Show)

data THFieldDef = THFieldDef {
    thFieldName :: String -- ^ name in the record, bar
  , thDbFieldName :: String -- ^ column name, SQLbar
  , thDbTypeName :: Maybe String -- ^ column type, inet, NUMERIC(5, 2), VARCHAR(50), etc.
  , thExprName :: String -- ^ name of constructor in the Field GADT, BarField
  , thFieldType :: Type
  , thEmbeddedDef :: Maybe [PSFieldDef String]
  , thDefaultValue :: Maybe String -- ^ default value in the database
  , thReferenceParent :: Maybe (Maybe ((Maybe String, String), [String]), Maybe ReferenceActionType, Maybe ReferenceActionType)
  , thFieldConverter :: Maybe String -- ^ name of a pair of functions
} deriving (Eq, Show)

data THUniqueDef = THUniqueDef {
    thUniqueName :: String
  , thUniqueType :: UniqueType
  , thUniqueFields :: [Either String String] -- ^ Either name of field, i.e, thFieldName, or expression
} deriving (Eq, Show)

data THUniqueKeyDef = THUniqueKeyDef {
    thUniqueKeyName :: String
  , thUniqueKeyPhantomName :: String
  , thUniqueKeyConstrName :: String
  , thUniqueKeyDbName :: String -- ^ It is used only to set polymorphic part of name of its container
    -- | It should repeat fields from THUniqueDef but it may give different settings for them. It is done to allow foreign key fields to be different from parent fields of the entity. These fields are used for creating a the key constructor and instances for it. For example, it can have a default value, or even a different type (INT8 may reference INT4).
  , thUniqueKeyFields :: [THFieldDef]
  , thUniqueKeyMakeEmbedded :: Bool -- ^ If True, make it an instance of Embedded
  , thUniqueKeyIsDef :: Bool
} deriving (Eq, Show)

data PSEntityDef = PSEntityDef {
    psDataName :: String -- SomeData
  , psDbEntityName :: Maybe String  -- SQLSomeData
  , psEntitySchema :: Maybe String
  , psAutoKey :: Maybe (Maybe PSAutoKeyDef) -- SomeDataKey. Nothing - default key. Just Nothing - no autokey. Just (Just _) - specify autokey settings
  , psUniqueKeys :: Maybe [PSUniqueKeyDef]
  , psConstructors :: Maybe [PSConstructorDef]
} deriving (Eq, Show)

data PSEmbeddedDef = PSEmbeddedDef {
    psEmbeddedName :: String
  , psDbEmbeddedName :: Maybe String -- ^ It is used only to set polymorphic part of name of its container
  , psEmbeddedFields :: Maybe [PSFieldDef String]
} deriving (Eq, Show)

data PSPrimitiveDef = PSPrimitiveDef {
    psPrimitiveName :: String
  , psPrimitiveDbName :: Maybe String -- ^ It is used only to set polymorphic part of name of its container
  , psPrimitiveStringEnumRepresentation :: Maybe Bool -- ^ Store in database as string using Show/Read instances (True) or as integer using Enum instance (False).
} deriving (Eq, Show)

data PSConstructorDef = PSConstructorDef {
    psConstrName :: String -- U2
  , psPhantomConstrName :: Maybe String -- U2Constructor
  , psDbConstrName :: Maybe String -- SQLU2
  , psDbAutoKeyName :: Maybe String -- u2_id
  , psConstrFields  :: Maybe [PSFieldDef String]
  , psConstrUniques :: Maybe [PSUniqueDef]
} deriving (Eq, Show)

data PSUniqueDef = PSUniqueDef {
    psUniqueName :: String
  , psUniqueType :: Maybe UniqueType
  , psUniqueFields :: [Either String String]
} deriving (Eq, Show)

data PSUniqueKeyDef = PSUniqueKeyDef {
    psUniqueKeyName :: String
  , psUniqueKeyPhantomName :: Maybe String
  , psUniqueKeyConstrName :: Maybe String
  , psUniqueKeyDbName :: Maybe String
  , psUniqueKeyFields :: Maybe [PSFieldDef String]
  , psUniqueKeyMakeEmbedded :: Maybe Bool
  , psUniqueKeyIsDef :: Maybe Bool
} deriving (Eq, Show)

data PSAutoKeyDef = PSAutoKeyDef {
    psAutoKeyConstrName :: Maybe String
  , psAutoKeyIsDef :: Maybe Bool
} deriving (Eq, Show)

instance Lift PSPrimitiveDef where
  lift (PSPrimitiveDef {..}) = [| PSPrimitiveDef $(lift psPrimitiveName) $(lift psPrimitiveDbName) $(lift psPrimitiveStringEnumRepresentation) |]

instance Lift PersistDefinitions where
  lift (PersistDefinitions {..}) = [| PersistDefinitions $(lift psEntities) $(lift psEmbeddeds) $(lift psPrimitives) |]

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
  lift (PSFieldDef {..}) = [| PSFieldDef $(lift psFieldName) $(lift psDbFieldName) $(lift psDbTypeName) $(lift psExprName) $(lift psEmbeddedDef) $(lift psDefaultValue) $(lift psReferenceParent) $(lift psFieldConverter) |]

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
  parseJSON value = (case value of
    Object v -> do
      defs <- v .:? "definitions"
      case defs of
        Just (Array arr) -> Fold.foldrM go initial arr
        Nothing -> go value initial
        Just _ -> mzero
    Array arr -> Fold.foldrM go initial arr
    _ -> mzero) where
    initial = PersistDefinitions [] [] []
    go obj p@(PersistDefinitions{..}) = flip (withObject "definition") obj $ \v -> case () of
      _ | H.member "entity" v -> (\x -> p {psEntities = x:psEntities}) <$> parseJSON obj
      _ | H.member "embedded" v -> (\x -> p {psEmbeddeds = x:psEmbeddeds}) <$> parseJSON obj
      _ | H.member "primitive" v -> (\x -> p {psPrimitives = x:psPrimitives}) <$> parseJSON obj
      _ -> fail $ "Invalid definition: " ++ show obj


instance FromJSON PSEntityDef where
  parseJSON = withObject "entity" $ \v ->
    PSEntityDef <$> v .: "entity" <*> v .:? "dbName" <*> v .:? "schema" <*> optional (v .: "autoKey") <*> v .:? "keys" <*> v .:? "constructors"

instance FromJSON PSEmbeddedDef where
  parseJSON = withObject "embedded" $ \v ->
    PSEmbeddedDef <$> v .: "embedded" <*> v .:? "dbName" <*> v .:? "fields"

instance FromJSON PSPrimitiveDef where
  parseJSON = withObject "primitive" $ \v -> do
    x <- v .:? "representation"
    let representation = case x of
          Nothing -> pure True
          Just "showread" -> pure True
          Just "enum" -> pure False
          Just r -> fail $ "parseJSON: representation expected [\"showread\",\"enum\"], but got " ++ r
    PSPrimitiveDef <$> v .: "primitive" <*> v .:? "dbName" <*> pure representation

instance FromJSON PSConstructorDef where
  parseJSON = withObject "constructor" $ \v ->
     PSConstructorDef <$> v .: "name" <*> v .:? "phantomName" <*> v .:? "dbName" <*> v .:? "keyDbName" <*> v .:? "fields" <*> v .:? "uniques"

instance FromJSON PSUniqueDef where
  parseJSON = withObject "unique" $ \v -> do
    fields <- v .: "fields"
    fields' <- forM fields $ \f -> case f of
      Object expr  -> Right <$> expr .: "expr"
      field -> Left <$> parseJSON field
    PSUniqueDef <$> v .: "name" <*> v .:? "type" <*> pure fields'

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
  parseJSON = withObject "field" $ \v ->
    PSFieldDef <$> v .: "name" <*> v .:? "dbName" <*> v .:? "type" <*> v .:? "exprName" <*> v .:? "embeddedType" <*> v .:? "default" <*> mkRefSettings v <*> v .:? "converter" where
    mkRefSettings v = do
      ref <- v .:? "reference"
      (parent, onDel, onUpd) <- case ref of
        Just (Object r) -> (,,) <$> optional parentRef <*> r .:? "onDelete" <*> r .:? "onUpdate" where
          parentRef = (,) <$> ((,) <$> r .:? "schema" <*> r .: "table") <*> r .: "columns"
        _ -> pure (Nothing, Nothing, Nothing)
      -- this temporary solution uses onDelete and onUpdate both from inside reference object (preferred) and from field level (for compatibility)
      (onDel', onUpd') <- (,) <$> v .:? "onDelete" <*> v .:? "onUpdate"
      pure $ case (parent, onDel <|> onDel', onUpd <|> onUpd') of
        (Nothing, Nothing, Nothing) -> Nothing
        refSettings -> Just refSettings

instance FromJSON PSUniqueKeyDef where
  parseJSON = withObject "unique key" $ \v ->
    PSUniqueKeyDef <$> v .: "name" <*> v .:? "keyPhantom" <*> v .:? "constrName" <*> v .:? "dbName" <*> v .:? "fields" <*> v .:? "mkEmbedded" <*> v .:? "default"

instance FromJSON PSAutoKeyDef where
  parseJSON = withObject "autogenerated key" $ \v ->
    PSAutoKeyDef <$> v .:? "constrName" <*> v .:? "default"

(.=?) :: ToJSON a => Text -> Maybe a -> Maybe Pair
name .=? value = (name .=) <$> value

(.=:) :: ToJSON a => Text -> Maybe [a] -> Maybe Pair
name .=: value = case value of
  Just (_:_) -> Just $ name .= value
  _          -> Nothing

instance ToJSON PSEntityDef where
  toJSON PSEntityDef{..} = object $ catMaybes [Just $ "entity" .= psDataName, "dbName" .=? psDbEntityName, "schema" .=? psEntitySchema, "autoKey" .=? psAutoKey, "keys" .=: psUniqueKeys, "constructors" .=: psConstructors]

instance ToJSON PSConstructorDef where
  toJSON PSConstructorDef{..} = object $ catMaybes [Just $ "name" .= psConstrName, "phantomName" .=? psPhantomConstrName, "dbName" .=? psDbConstrName, "keyDbName" .=? psDbAutoKeyName, "fields" .=: psConstrFields, "uniques" .=: psConstrUniques]

instance ToJSON PSUniqueDef where
  toJSON PSUniqueDef{..} = object $ catMaybes [Just $ "name" .= psUniqueName, "type" .=? psUniqueType, "fields" .=? fields] where
    fields = if null psUniqueFields
      then Nothing
      else Just $ map (either toJSON (\x -> object ["expr" .= x])) psUniqueFields

instance ToJSON UniqueType where
  toJSON a = toJSON $ case a of
    UniqueConstraint -> "constraint" :: String
    UniqueIndex      -> "index"
    UniquePrimary _  -> "primary"

instance ToJSON ReferenceActionType where
  toJSON a = toJSON $ case a of
    NoAction   -> "no action" :: String
    Restrict   -> "restrict"
    Cascade    -> "cascade"
    SetNull    -> "set null"
    SetDefault -> "set default"

instance ToJSON (PSFieldDef String) where
  toJSON PSFieldDef{..} = object $ catMaybes [Just $ "name" .= psFieldName, "dbName" .=? psDbFieldName, "type" .=? psDbTypeName, "exprName" .=? psExprName, "embeddedType" .=: psEmbeddedDef, "default" .=? psDefaultValue, "reference" .=? (psReferenceParent >>= mkRefSettings)] where
    mkRefSettings (parent, onDel, onUpd) = if null fields then Nothing else Just $ object fields where
      fields = catMaybes $ parent' ++ ["onDelete" .=? onDel, "onUpdate" .=? onUpd]
      parent' = case parent of
        Nothing -> []
        Just ((schema, table), columns) -> ["schema" .=? schema, Just $ "table" .= table, Just $ "columns" .= columns]

instance ToJSON PSUniqueKeyDef where
  toJSON PSUniqueKeyDef{..} = object $ catMaybes [Just $ "name" .= psUniqueKeyName, "keyPhantom" .=? psUniqueKeyPhantomName, "constrName" .=? psUniqueKeyConstrName, "dbName" .=? psUniqueKeyDbName, "mkEmbedded" .=? psUniqueKeyMakeEmbedded, "default" .=? psUniqueKeyIsDef, "fields" .=: psUniqueKeyFields]

instance ToJSON PSAutoKeyDef where
  toJSON PSAutoKeyDef{..} = object $ catMaybes ["constrName" .=? psAutoKeyConstrName, "default" .=? psAutoKeyIsDef]
