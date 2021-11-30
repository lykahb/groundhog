{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functions to generate the auxiliary structures for the user data type
module Database.Groundhog.TH
  ( -- * Settings format
    -- $settingsDoc
    mkPersist,
    groundhog,
    groundhogFile,

    -- * Settings for code generation
    CodegenConfig (..),
    defaultCodegenConfig,
    defaultMkEntityDecs,
    defaultMkEmbeddedDecs,
    defaultMkPrimitiveDecs,
    -- $namingStylesDoc
    NamingStyle (..),
    suffixNamingStyle,
    persistentNamingStyle,
    conciseNamingStyle,
    lowerCaseSuffixNamingStyle,
    toUnderscore,
    firstChar,

    -- * Utility functions
    mkTHEntityDef,
    mkTHEmbeddedDef,
    mkTHPrimitiveDef,
    applyEntitySettings,
    applyEmbeddedSettings,
    applyPrimitiveSettings,

    -- * Helpers
    showReadConverter,
    enumConverter,
  )
where

import Control.Applicative
import Control.Monad (forM, forM_, liftM2, unless, when)
import Data.Char (isDigit, isLower, isSpace, isUpper, toLower, toUpper)
import Data.List (intercalate, nub, (\\))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml as Y (ParseException (..), decodeHelper)
import Database.Groundhog.Core (UniqueType (..), delim)
import Database.Groundhog.Generic
import Database.Groundhog.TH.CodeGen
import Database.Groundhog.TH.Settings
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift (..), StrictType, VarStrictType)
import qualified Text.Libyaml as Y

data CodegenConfig = CodegenConfig
  { -- | Naming style that is applied for all definitions
    namingStyle :: NamingStyle,
    -- | Codegenerator will create a function with this name that will run 'migrate' for each non-polymorphic entity in definition
    migrationFunction :: Maybe String,
    -- | Functions that produce Haskell code for the entities. In most cases when overriding, the default functions that produce mappings are not replaced but kept along with custom code. Example: @['defaultMkEntityDecs', mkMyDecs]@.
    mkEntityDecs :: [[THEntityDef] -> Q [Dec]],
    mkEmbeddedDecs :: [[THEmbeddedDef] -> Q [Dec]],
    mkPrimitiveDecs :: [[THPrimitiveDef] -> Q [Dec]]
  }

defaultCodegenConfig :: CodegenConfig
defaultCodegenConfig = CodegenConfig suffixNamingStyle Nothing [defaultMkEntityDecs] [defaultMkEmbeddedDecs] [defaultMkPrimitiveDecs]

-- $namingStylesDoc
-- When describing a datatype you can omit the most of the declarations.
-- In this case the omitted parts of description will be automatically generated using the default names created by naming style.
-- Any default name can be overridden by setting its value explicitly.

-- | Defines how the names are created. The mk* functions correspond to the set* functions.
-- Functions mkNormal* define names of non-record constructor Field
data NamingStyle = NamingStyle
  { -- | Create name of the table for the datatype. Parameters: data name.
    mkDbEntityName :: String -> String,
    -- | Create name of the backend-specific key constructor for the datatype. Parameters: data name.
    mkEntityKeyName :: String -> String,
    -- | Create name for phantom constructor used to parametrise 'Field'. Parameters: data name, constructor name, constructor position.
    mkPhantomName :: String -> String -> Int -> String,
    -- | Create name for phantom unique key used to parametrise 'Key'. Parameters: data name, constructor name, unique constraint name.
    mkUniqueKeyPhantomName :: String -> String -> String -> String,
    -- | Create name of constructor for the unique key. Parameters: data name, constructor name, unique constraint name.
    mkUniqueKeyConstrName :: String -> String -> String -> String,
    -- | Create name used by 'persistName' for the unique key. Parameters: data name, constructor name, unique constraint name.
    mkUniqueKeyDbName :: String -> String -> String -> String,
    -- | Create name of the constructor specific table. Parameters: data name, constructor name, constructor position.
    mkDbConstrName :: String -> String -> Int -> String,
    -- | Create name of the db field for autokey. Parameters: data name, constructor name, constructor position.
    mkDbConstrAutoKeyName :: String -> String -> Int -> String,
    -- | Create name of the field column in a database. Parameters: data name, constructor name, constructor position, field record name, field position.
    mkDbFieldName :: String -> String -> Int -> String -> Int -> String,
    -- | Create name of field constructor used in expressions. Parameters: data name, constructor name, constructor position, field record name, field position.
    mkExprFieldName :: String -> String -> Int -> String -> Int -> String,
    -- | Create name of selector (see 'Embedded') constructor used in expressions. Parameters: data name, constructor name, field record name, field position.
    mkExprSelectorName :: String -> String -> String -> Int -> String,
    -- | Create field name used to refer to the it in settings for non-record constructors. Parameters: data name, constructor name, constructor position, field position.
    mkNormalFieldName :: String -> String -> Int -> Int -> String,
    -- | Create name of the field column in a database. Parameters: data name, constructor name, constructor position, field position.
    mkNormalDbFieldName :: String -> String -> Int -> Int -> String,
    -- | Create name of field constructor used in expressions. Parameters: data name, constructor name, constructor position, field position.
    mkNormalExprFieldName :: String -> String -> Int -> Int -> String,
    -- | Create name of selector (see 'Embedded') constructor used in expressions. Parameters: data name, constructor name, field position.
    mkNormalExprSelectorName :: String -> String -> Int -> String
  }

-- | Default style. Adds \"Field\" to each record field name.
--
-- Example:
--
-- > data SomeData a = Normal Int | Record { bar :: Maybe String, asc :: a}
-- > -- Generated code
-- > data NormalConstructor
-- > data RecordConstructor
-- > instance PersistEntity where
-- >   data Field (SomeData a) where
-- >     Normal0Field :: Field NormalConstructor Int
-- >     BarField :: Field RecordConstructor (Maybe String)
-- >     AscField :: Field RecordConstructor a
-- > ...
suffixNamingStyle :: NamingStyle
suffixNamingStyle =
  NamingStyle
    { mkDbEntityName = \dName -> dName,
      mkEntityKeyName = \dName -> dName ++ "Key",
      mkPhantomName = \_ cName _ -> cName ++ "Constructor",
      mkUniqueKeyPhantomName = \_ _ uName -> firstChar toUpper uName,
      mkUniqueKeyConstrName = \_ _ uName -> firstChar toUpper uName ++ "Key",
      mkUniqueKeyDbName = \_ _ uName -> "Key" ++ [delim] ++ firstChar toUpper uName,
      mkDbConstrName = \_ cName _ -> cName,
      mkDbConstrAutoKeyName = \_ _ _ -> "id",
      mkDbFieldName = \_ _ _ fName _ -> fName,
      mkExprFieldName = \_ _ _ fName _ -> firstChar toUpper fName ++ "Field",
      mkExprSelectorName = \_ _ fName _ -> firstChar toUpper fName ++ "Selector",
      mkNormalFieldName = \_ cName _ fNum -> firstChar toLower cName ++ show fNum,
      mkNormalDbFieldName = \_ cName _ fNum -> firstChar toLower cName ++ show fNum,
      mkNormalExprFieldName = \_ cName _ fNum -> cName ++ show fNum ++ "Field",
      mkNormalExprSelectorName = \_ cName fNum -> cName ++ show fNum ++ "Selector"
    }

-- | Creates field names in Persistent fashion prepending constructor names to the fields.
--
-- Example:
--
-- > data SomeData a = Normal Int | Record { bar :: Maybe String, asc :: a}
-- > -- Generated code
-- > data NormalConstructor
-- > data RecordConstructor
-- > instance PersistEntity where
-- >   data Field (SomeData a) where
-- >     Normal0 :: Field NormalConstructor Int
-- >     RecordBar :: Field RecordConstructor (Maybe String)
-- >     RecordAsc :: Field RecordConstructor a
-- > ...
persistentNamingStyle :: NamingStyle
persistentNamingStyle =
  suffixNamingStyle
    { mkExprFieldName = \_ cName _ fName _ -> cName ++ firstChar toUpper fName,
      mkExprSelectorName = \_ cName fName _ -> cName ++ firstChar toUpper fName,
      mkNormalExprFieldName = \_ cName _ fNum -> cName ++ show fNum,
      mkNormalExprSelectorName = \_ cName fNum -> cName ++ show fNum
    }

-- | Creates the shortest field names. It is more likely to lead in name conflicts than other naming styles.
--
-- Example:
--
-- > data SomeData a = Normal Int | Record { bar :: Maybe String, asc :: a}
-- > -- Generated code
-- > data NormalConstructor
-- > data RecordConstructor
-- > instance PersistEntity where
-- >   data Field (SomeData a) where
-- >     Normal0 :: Field NormalConstructor Int
-- >     Bar :: Field RecordConstructor (Maybe String)
-- >     Asc :: Field RecordConstructor a
-- > ...
conciseNamingStyle :: NamingStyle
conciseNamingStyle =
  suffixNamingStyle
    { mkExprFieldName = \_ _ _ fName _ -> firstChar toUpper fName,
      mkExprSelectorName = \_ _ fName _ -> firstChar toUpper fName,
      mkNormalExprFieldName = \_ cName _ fNum -> cName ++ show fNum,
      mkNormalExprSelectorName = \_ cName fNum -> cName ++ show fNum
    }

-- | The generated Haskell names of phantom types (constructors, fields, etc.) are the same as with suffixNamingStyle. But the table names and columns are converted from camelCase to underscore_lower_case with `toUnderscore`.
lowerCaseSuffixNamingStyle :: NamingStyle
lowerCaseSuffixNamingStyle =
  suffixNamingStyle
    { mkDbEntityName = \dName -> toUnderscore dName,
      mkDbConstrName = \_ cName _ -> toUnderscore cName,
      mkDbFieldName = \_ _ _ fName _ -> toUnderscore fName,
      mkNormalDbFieldName = \_ cName _ fNum -> toUnderscore $ cName ++ show fNum
    }

-- | Creates the auxiliary structures.
-- Particularly, it creates GADT 'Field' data instance for referring to the fields in expressions and phantom types for data constructors.
-- The default names of auxiliary datatypes and names used in database are generated using the naming style and can be changed via configuration.
-- The datatypes and their generation options are defined via YAML configuration parsed by quasiquoter 'groundhog'.
mkPersist :: CodegenConfig -> PersistDefinitions -> Q [Dec]
mkPersist CodegenConfig {..} PersistDefinitions {..} = do
  checkEnabledLanguageExtensions
  let duplicates =
        notUniqueBy id $
          map psDataName psEntities ++ map psEmbeddedName psEmbeddeds ++ map psPrimitiveName psPrimitives
  unless (null duplicates) $ fail $ "All definitions must be unique. Found duplicates: " ++ show duplicates
  let getDecl name = do
        info <- reify $ mkName name
        return $ case info of
          TyConI d -> d
          _ -> error $ "Only datatypes can be processed: " ++ name

  entities <- forM psEntities $ \e ->
    either error id . validateEntity . applyEntitySettings namingStyle e . mkTHEntityDef namingStyle <$> getDecl (psDataName e)
  embeddeds <- forM psEmbeddeds $ \e ->
    either error id . validateEmbedded . applyEmbeddedSettings e . mkTHEmbeddedDef namingStyle <$> getDecl (psEmbeddedName e)
  primitives <- forM psPrimitives $ \e ->
    applyPrimitiveSettings e . mkTHPrimitiveDef namingStyle <$> getDecl (psPrimitiveName e)
  let mkEntityDecs' = maybe id (\name -> (mkMigrateFunction name :)) migrationFunction mkEntityDecs
  fmap concat $ sequence $ map ($ entities) mkEntityDecs' ++ map ($ embeddeds) mkEmbeddedDecs ++ map ($ primitives) mkPrimitiveDecs

applyEntitySettings :: NamingStyle -> PSEntityDef -> THEntityDef -> THEntityDef
applyEntitySettings style PSEntityDef {..} def@THEntityDef {..} =
  def
    { thDbEntityName = fromMaybe thDbEntityName psDbEntityName,
      thEntitySchema = psEntitySchema,
      thAutoKey = thAutoKey',
      thUniqueKeys = maybe thUniqueKeys (map mkUniqueKey') psUniqueKeys,
      thConstructors = thConstructors'
    }
  where
    thAutoKey' = maybe thAutoKey (liftM2 applyAutoKeySettings thAutoKey) psAutoKey
    thConstructors' = maybe thConstructors'' (f thConstructors'') psConstructors
      where
        thConstructors'' = map checkAutoKey thConstructors
        checkAutoKey cDef@THConstructorDef {..} = cDef {thDbAutoKeyName = thAutoKey' >> thDbAutoKeyName}

    mkUniqueKey' = mkUniqueKey style (nameBase thDataName) (head thConstructors')
    f = foldr $ replaceOne "constructor" psConstrName (nameBase . thConstrName) applyConstructorSettings

mkUniqueKey :: NamingStyle -> String -> THConstructorDef -> PSUniqueKeyDef -> THUniqueKeyDef
mkUniqueKey style@NamingStyle {..} dName cDef@THConstructorDef {..} PSUniqueKeyDef {..} = key
  where
    key =
      THUniqueKeyDef
        { thUniqueKeyName = psUniqueKeyName,
          thUniqueKeyPhantomName = fromMaybe (mkUniqueKeyPhantomName dName (nameBase thConstrName) psUniqueKeyName) psUniqueKeyPhantomName,
          thUniqueKeyConstrName = fromMaybe (mkUniqueKeyConstrName dName (nameBase thConstrName) psUniqueKeyName) psUniqueKeyConstrName,
          thUniqueKeyDbName = fromMaybe (mkUniqueKeyDbName dName (nameBase thConstrName) psUniqueKeyName) psUniqueKeyDbName,
          thUniqueKeyFields = maybe uniqueFields (f uniqueFields) psUniqueKeyFields,
          thUniqueKeyMakeEmbedded = fromMaybe False psUniqueKeyMakeEmbedded,
          thUniqueKeyIsDef = fromMaybe False psUniqueKeyIsDef
        }
    f = foldr $ replaceOne "unique field" psFieldName thFieldName applyFieldSettings
    uniqueFields = mkFieldsForUniqueKey style dName key cDef

applyAutoKeySettings :: THAutoKeyDef -> PSAutoKeyDef -> THAutoKeyDef
applyAutoKeySettings def@THAutoKeyDef {..} PSAutoKeyDef {..} =
  def
    { thAutoKeyConstrName = fromMaybe thAutoKeyConstrName psAutoKeyConstrName,
      thAutoKeyIsDef = fromMaybe thAutoKeyIsDef psAutoKeyIsDef
    }

applyConstructorSettings :: PSConstructorDef -> THConstructorDef -> THConstructorDef
applyConstructorSettings PSConstructorDef {..} def@THConstructorDef {..} =
  def
    { thPhantomConstrName = fromMaybe thPhantomConstrName psPhantomConstrName,
      thDbConstrName = fromMaybe thDbConstrName psDbConstrName,
      thDbAutoKeyName = psDbAutoKeyName <|> thDbAutoKeyName,
      thConstrFields = maybe thConstrFields (f thConstrFields) psConstrFields,
      thConstrUniques = maybe thConstrUniques (map convertUnique) psConstrUniques
    }
  where
    f = foldr $ replaceOne "field" psFieldName thFieldName applyFieldSettings
    convertUnique (PSUniqueDef uName uType uFields) = THUniqueDef uName (fromMaybe UniqueConstraint uType) uFields

applyFieldSettings :: PSFieldDef String -> THFieldDef -> THFieldDef
applyFieldSettings PSFieldDef {..} def@THFieldDef {..} =
  def
    { thDbFieldName = fromMaybe thDbFieldName psDbFieldName,
      thExprName = fromMaybe thExprName psExprName,
      thDbTypeName = psDbTypeName,
      thEmbeddedDef = psEmbeddedDef,
      thDefaultValue = psDefaultValue,
      thReferenceParent = psReferenceParent,
      thFieldConverter = fmap mkName psFieldConverter
    }

applyEmbeddedSettings :: PSEmbeddedDef -> THEmbeddedDef -> THEmbeddedDef
applyEmbeddedSettings PSEmbeddedDef {..} def@THEmbeddedDef {..} =
  def
    { thDbEmbeddedName = fromMaybe thDbEmbeddedName psDbEmbeddedName,
      thEmbeddedFields = maybe thEmbeddedFields (f thEmbeddedFields) psEmbeddedFields
    }
  where
    f = foldr $ replaceOne "field" psFieldName thFieldName applyFieldSettings

applyPrimitiveSettings :: PSPrimitiveDef -> THPrimitiveDef -> THPrimitiveDef
applyPrimitiveSettings PSPrimitiveDef {..} def@THPrimitiveDef {..} =
  def
    { thPrimitiveDbName = fromMaybe thPrimitiveDbName psPrimitiveDbName,
      thPrimitiveConverter = mkName psPrimitiveConverter
    }

mkFieldsForUniqueKey :: NamingStyle -> String -> THUniqueKeyDef -> THConstructorDef -> [THFieldDef]
mkFieldsForUniqueKey style dName uniqueKey cDef = zipWith (setSelector . findField) (thUniqueFields uniqueDef) [0 ..]
  where
    findField (Left name) = findOne "field" thFieldName name $ thConstrFields cDef
    findField (Right expr) = error $ "A unique key may not contain expressions: " ++ expr
    uniqueDef = findOne "unique" thUniqueName (thUniqueKeyName uniqueKey) $ thConstrUniques cDef
    setSelector f i = f {thExprName = mkExprSelectorName style dName (thUniqueKeyConstrName uniqueKey) (thFieldName f) i}

notUniqueBy :: Eq b => (a -> b) -> [a] -> [b]
notUniqueBy f xs = let xs' = map f xs in nub $ xs' \\ nub xs'

assertUnique :: (Eq b, Show b) => (a -> b) -> [a] -> String -> Either String ()
assertUnique f xs what = case notUniqueBy f xs of
  [] -> return ()
  ys -> Left $ "All " ++ what ++ " must be unique: " ++ show ys

-- we need to validate datatype names because TH just creates unusable fields with spaces
assertSpaceFree :: String -> String -> Either String ()
assertSpaceFree s what = when (any isSpace s) $ Left $ "Spaces in " ++ what ++ " are not allowed: " ++ show s

validateEntity :: THEntityDef -> Either String THEntityDef
validateEntity def = do
  let constrs = thConstructors def
  assertUnique thPhantomConstrName constrs "constructor phantom name"
  assertUnique thDbConstrName constrs "constructor db name"
  forM_ constrs $ \cdef -> do
    let fields = thConstrFields cdef
    assertSpaceFree (thPhantomConstrName cdef) "constructor phantom name"
    assertUnique thExprName fields "expr field name in a constructor"
    assertUnique thDbFieldName fields "db field name in a constructor"
    mapM_ validateField fields
    case filter (\(THUniqueDef _ _ uFields) -> null uFields) $ thConstrUniques cdef of
      [] -> return ()
      ys -> Left $ "Constraints must have at least one field: " ++ show ys
    when (isNothing (thDbAutoKeyName cdef) /= isNothing (thAutoKey def)) $
      Left $ "Presence of autokey definitions should be the same in entity and constructors definitions " ++ show (thDataName def) ++ ": " ++ show (thDbAutoKeyName cdef) ++ " - " ++ show (thAutoKey def)

  -- check that unique keys = [] for multiple constructor datatype
  if length constrs > 1 && not (null $ thUniqueKeys def)
    then Left $ "Unique keys may exist only for datatypes with single constructor: " ++ show (thDataName def)
    else -- check that all unique keys reference existing uniques

      let uniqueNames = map thUniqueName $ thConstrUniques $ head constrs
       in forM_ (thUniqueKeys def) $ \cKey ->
            unless (thUniqueKeyName cKey `elem` uniqueNames) $
              Left $ "Unique key mentions unknown unique: " ++ thUniqueKeyName cKey ++ " in datatype " ++ show (thDataName def)
  let isPrimary x = case x of
        UniquePrimary _ -> True
        _ -> False
      primaryConstraints = length $ filter (isPrimary . thUniqueType) $ concatMap thConstrUniques constrs
  if length constrs > 1
    then
      when (primaryConstraints > 0) $
        Left $ "Custom primary keys may exist only for datatypes with single constructor: " ++ show (thDataName def)
    else
      when (primaryConstraints + maybe 0 (const 1) (thAutoKey def) > 1) $
        Left $ "A datatype cannot have more than one primary key constraint: " ++ show (thDataName def)
  -- check that only one of the keys is default
  let keyDefaults = maybe id ((:) . thAutoKeyIsDef) (thAutoKey def) $ map thUniqueKeyIsDef (thUniqueKeys def)
  when (not (null keyDefaults) && length (filter id keyDefaults) /= 1) $
    Left $ "A datatype with keys must have one default key: " ++ show (thDataName def)
  return def

validateField :: THFieldDef -> Either String ()
validateField fDef = do
  assertSpaceFree (thExprName fDef) "field expr name"
  when (isJust (thDbTypeName fDef) && isJust (thEmbeddedDef fDef)) $
    Left $ "A field may not have both type and embeddedType: " ++ show (thFieldName fDef)

validateEmbedded :: THEmbeddedDef -> Either String THEmbeddedDef
validateEmbedded def = do
  let fields = thEmbeddedFields def
  assertUnique thExprName fields "expr field name in an embedded datatype"
  assertUnique thDbFieldName fields "db field name in an embedded datatype"
  mapM_ validateField fields
  return def

mkTHEntityDef :: NamingStyle -> Dec -> THEntityDef
mkTHEntityDef NamingStyle {..} dec = THEntityDef dName (mkDbEntityName dName') Nothing autokey [] typeVars constrs
  where
    (dName, typeVars, cons) = fromDataD dec
    constrs = zipWith mkConstr [0 ..] cons
    dName' = nameBase dName
    autokey = Just $ THAutoKeyDef (mkEntityKeyName dName') True

    mkConstr cNum c = case c of
      NormalC name params -> mkConstr' name $ zipWith (mkField (nameBase name)) params [0 ..]
      RecC name params -> mkConstr' name $ zipWith (mkVarField (nameBase name)) params [0 ..]
      _ -> error $ "Only regular types and records are supported" ++ show dName
      where
        mkConstr' name params = THConstructorDef name (apply mkPhantomName) (apply mkDbConstrName) (Just $ apply mkDbConstrAutoKeyName) params []
          where
            apply f = f dName' (nameBase name) cNum

        mkField :: String -> StrictType -> Int -> THFieldDef
        mkField cName (_, t) fNum = THFieldDef (apply mkNormalFieldName) (apply mkNormalDbFieldName) Nothing (apply mkNormalExprFieldName) t Nothing Nothing Nothing Nothing
          where
            apply f = f dName' cName cNum fNum
        mkVarField :: String -> VarStrictType -> Int -> THFieldDef
        mkVarField cName (fName, _, t) fNum = THFieldDef fName' (apply mkDbFieldName) Nothing (apply mkExprFieldName) t Nothing Nothing Nothing Nothing
          where
            apply f = f dName' cName cNum fName' fNum
            fName' = nameBase fName

mkTHEmbeddedDef :: NamingStyle -> Dec -> THEmbeddedDef
mkTHEmbeddedDef NamingStyle {..} dec = THEmbeddedDef dName cName (mkDbEntityName dName') typeVars fields
  where
    (dName, typeVars, cons) = fromDataD dec
    dName' = nameBase dName

    (cName, fields) = case cons of
      [cons'] -> case cons' of
        NormalC name params -> (name, zipWith (mkField (nameBase name)) params [0 ..])
        RecC name params -> (name, zipWith (mkVarField (nameBase name)) params [0 ..])
        _ -> error $ "Only regular types and records are supported" ++ show dName
      _ -> error $ "An embedded datatype must have exactly one constructor: " ++ show dName

    mkField :: String -> StrictType -> Int -> THFieldDef
    mkField cName' (_, t) fNum = THFieldDef (apply mkNormalFieldName) (apply mkNormalDbFieldName) Nothing (mkNormalExprSelectorName dName' cName' fNum) t Nothing Nothing Nothing Nothing
      where
        apply f = f dName' cName' 0 fNum
    mkVarField :: String -> VarStrictType -> Int -> THFieldDef
    mkVarField cName' (fName, _, t) fNum = THFieldDef fName' (apply mkDbFieldName) Nothing (mkExprSelectorName dName' cName' fName' fNum) t Nothing Nothing Nothing Nothing
      where
        apply f = f dName' cName' 0 fName' fNum
        fName' = nameBase fName

mkTHPrimitiveDef :: NamingStyle -> Dec -> THPrimitiveDef
mkTHPrimitiveDef NamingStyle {..} dec = THPrimitiveDef dName (mkDbEntityName dName') 'showReadConverter
  where
    dName = case dec of
#if MIN_VERSION_template_haskell(2, 11, 0)
      DataD _ name _ _ _ _ -> name
      NewtypeD _ name _ _ _ _ -> name
#else
      DataD _ name _ _ _ -> name
      NewtypeD _ name _ _ _ -> name
#endif
      _ -> error $ "Only datatypes and newtypes can be declared as primitive: " ++ show dec
    dName' = nameBase dName

showReadConverter :: (Show a, Read a) => (a -> String, String -> a)
showReadConverter = (show, read)

enumConverter :: Enum a => (a -> Int, Int -> a)
enumConverter = (fromEnum, toEnum)

firstChar :: (Char -> Char) -> String -> String
firstChar f s = f (head s) : tail s

-- | Transforms string from camelCase to lower_case_underscore naming convention.
-- ColumnName -> column_name, parseURL -> parse_url, FieldIEEE754Floating -> field_ieee754_floating
toUnderscore :: String -> String
toUnderscore = map toLower . go
  where
    go (x : y : z : xs) | isUpper x && isUpper y && isLower z = x : '_' : y : go (z : xs)
    go (x : y : xs) | (isLower x || isDigit x) && isUpper y = x : '_' : y : go xs
    go (x : xs) = x : go xs
    go "" = ""

-- $settingsDoc
-- Groundhog needs to analyze the datatypes and create the auxiliary definitions before it can work with them.
-- We use YAML-based settings to list the datatypes and adjust the result of their introspection.
--
-- A datatype can be treated as entity or embedded. An entity is stored in its own table, can be referenced in fields of other data, etc. It is a first-class value.
-- An embedded type can only be a field of an entity or another embedded type. For example, the tuples are embedded.
-- You can create your own embedded types and adjust the fields names of an existing embedded type individually for any place where it is used.
--
-- Unless the property is marked as mandatory, it can be omitted. In this case value created by the NamingStyle will be used.
--
-- @
-- data Settable = First {foo :: String, bar :: Int, next :: Maybe (Key Settable BackendSpecific)} deriving (Eq, Show)
--
--    \-- The declaration with defaulted names
--
-- mkPersist defaultCodegenConfig [groundhog|
-- entity: Settable                       # If we did not have a constraint, this line would be enough
-- keys:
--  - name: someconstraint
-- constructors:
--  - name: First
--    uniques:
--      - name: someconstraint
--        fields: [foo, bar]
-- |]
-- @
--
-- Which is equivalent to the example below that has all properties set explicitly.
--
-- @
-- mkPersist defaultCodegenConfig [groundhog|
-- definitions:                           # First level key whose value is a list of definitions. It can be considered an optional header.
--                                       # The list elements start with hyphen+space. Keys are separated from values by a colon+space. See full definition at http://yaml.org/spec/1.2/spec.html.
--  - entity: Settable                   # Mandatory. Entity datatype name
--    dbName: Settable                   # Name of the main table
--    \# schema: public                   # Name of the schema to which the table belongs
--    autoKey:                           # Description of the autoincremented key for data family Key instance
--      constrName: SettableKey          # Name of constructor
--      default: true                    # The default key is used when entity is referenced without key wrapper. E.g., \"field :: SomeData\" instead of \"field :: Key SomeData keytype\"
--    keys:                              # List of the unique keys. An entity may have unique keys only if it has one constructor
--      - name: someconstraint           # This name references names from uniques field of constructor
--        keyPhantom: Someconstraint     # Name of phantom datatype that corresponds for each unique key
--        constrName: SomeconstraintKey  # Name of data family Key instance constructor for this unique key
--        dbName: Key\#Someconstraint     # It is used for function \"persistName\" of \"PersistField (Key Settable (Unique Someconstraint))\"
--        fields: []                     # Set fields that comprise this unique constraint. It works like setting fields in constructors
--        mkEmbedded: false              # Defines if instance of \"Embedded (Key Settable (Unique Someconstraint))\" will be created. The \"Selector\" constructor names are defined by properties of key fields.
--        default: false                 # Defines if this unique key is used as default
--    constructors:                      # List of constructors. The constructors you don't change can be omitted
--      - name: First                    # Mandatory. Constructor name
--        phantomName: FooBarConstructor # Constructor phantom type name used to guarantee type safety
--        dbName: First                  # Name of constructor table which is created only for datatypes with multiple constructors
--        keyDbName: id                  # Name for the primary key column
--        fields:                        # List of constructor fields. If you don't change a field, you can omit it
--          - name: foo                  # The name as in constructor record. If constructor is not a record, the name is created by 'mkNormalFieldName'. For example, the fields in constructor SomeConstr would have names someConstr0 and someConstr1 by default.
--            dbName: foo                # Column name
--            exprName: FooField         # Name of a field used in expressions
--          \# type: varchar              # This would result in having field type DbOther \"varchar\" instead of DbString. Value of this attribute will be used by DB backend for migration
--          \# default: foo_value         # The default value for column in the clause
--          \# reference:                 # This is explicit reference to a parent table not mapped by Groundhog
--          \#   schema: myschema         # Optional schema
--          \#   table: mytable           # Name of the parent table
--          \#   columns: [mytable_id]    # Parent columns. If the current field is embedded, e.g., a tuple, it will be a composite key
--          \#   onDelete: cascade        # Defines ON DELETE clause of references. It can have values: no action, restrict, cascade, set null, set default
--          \#   onUpdate: restrict       # Defines ON UPDATE
--          \# onDelete: cascade          # Clauses onDelete and onUpdate can be set outside of reference too. This is deprecated and kept for compatibility
--          \# If onDelete or onUpdate are omitted, the database will choose the action automatically. Note that it may differ across databases.
--          \# For example, MySQL has \"restrict\" by default, but in PostgreSQL it is \"no action\".
--          - name: bar
--            dbName: bar
--            exprName: BarField
--                                       # For some databases \"type: integer\" would be appropriate
--          - name: next
--            dbName: next
--            exprName: NextField
--        uniques:
--          - name: someconstraint
--            type: constraint           # The type can be be \"constraint\", \"index\", or \"primary\"
--            fields: [foo, bar]         # List of constructor parameter names. Not column names.
--   # This is example for databases which support expression indexes.
--   # Note that for checking index during migration expression should be written in exactly the same form as database returns.
--   #  - name: myuniqueindex
--   #    type: index
--   #    fields: [foo, {expr: "(bar + 1)" }]
-- |]
-- @
--
-- This is an example of embedded datatype usage.
--
-- @
-- data Company = Company {name :: String, headquarter :: Address, dataCentre :: Address, salesOffice :: Address} deriving (Eq, Show)
-- data Address = Address {city :: String, zipCode :: String, street :: String} deriving (Eq, Show)
--
-- mkPersist defaultCodegenConfig [groundhog|
-- definitions:
--  - entity: Company
--    constructors:
--      - name: Company
--        fields:
--                                        # Property embeddedType of headquarter field is not mentioned, so the corresponding table columns will have names prefixed with headquarter (headquarter$city, headquarter$zip_code, headquarter$street)
--          - name: dataCentre
--            embeddedType:               # If a field has an embedded type you can access its subfields. If you do it, the database columns will match with the embedded dbNames (no prefixing).
--              - name: city              # Just a regular list of fields. However, note that you should use default dbNames of embedded
--                dbName: dc_city
--              - name: zip_code          # Here we use embedded dbName (zip_code) which differs from the name used in Address definition (zipCode) for accessing the field.
--                dbName: dc_zipcode
--              - name: street
--                dbName: dc_street
--          - name: salesOffice
--            embeddedType:               # Similar declaration, but using another syntax for YAML objects
--              - {name: city, dbName: sales_city}
--              - {name: zip_code, dbName: sales_zipcode}
--              - {name: street, dbName: sales_street}
--  - embedded: Address
--    fields:                             # The syntax is the same as for constructor fields. Nested embedded types are allowed.
--      - name: city                      # This line does nothing and can be omitted. Default settings for city are not changed.
--      - name: zipCode
--        dbName: zip_code                # Change column name.
--                                        # Street is not mentioned so it will have default settings.
-- |]
-- @
--
-- We can also make our types instances of `PrimitivePersistField` to store them in one column.
--
-- @
-- data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
--  deriving (Eq, Show, Enum)
-- data Point = Point Int Int
--  deriving (Eq, Show, Read)
--
-- mkPersist defaultCodegenConfig [groundhog|
-- definitions:
--  - primitive: WeekDay
--    converter: enumConverter            # Its column will have integer type. The conversion will use Enum instance.
--  - primitive: Point
--    converter: showReadConverter        # Its column will have string type. The conversion will use Show/Read instances. If representation is omitted, showread will be used by default.
-- |]
-- @

-- | Converts quasiquoted settings into the datatype used by mkPersist.
groundhog :: QuasiQuoter
groundhog =
  QuasiQuoter
    { quoteExp = parseDefinitions,
      quotePat = error "groundhog: pattern quasiquoter",
      quoteType = error "groundhog: type quasiquoter",
      quoteDec = error "groundhog: declaration quasiquoter"
    }

-- | Parses configuration stored in the file
--
-- > mkPersist defaultCodegenConfig [groundhogFile|../groundhog.yaml|]
groundhogFile :: QuasiQuoter
groundhogFile = quoteFile groundhog

parseDefinitions :: String -> Q Exp
parseDefinitions s = do
  result <- runIO $ decodeHelper (Y.decode $ encodeUtf8 $ fromString s)
  case result of
    Left err -> case err of
      InvalidYaml (Just (Y.YamlParseException problem context mark)) ->
        fail $
          unlines
            [ "YAML parse error: " ++ problem,
              "Context: " ++ context,
              "At line: " ++ show (Y.yamlLine mark),
              lines s !! Y.yamlLine mark,
              replicate (Y.yamlColumn mark) ' ' ++ "^"
            ]
      _ -> fail $ show err
    Right (_, Left err) -> fail err
    Right (_, Right result') -> lift (result' :: PersistDefinitions)

checkEnabledLanguageExtensions :: Q ()
checkEnabledLanguageExtensions = do
  exts <- extsEnabled
  let missingExtensions = map show (requiredLanguageExtensions \\ exts)
  unless (null missingExtensions) $
    fail $
      "Groundhog requires that you enable additionally the following language extensions: "
        ++ intercalate ", " missingExtensions

requiredLanguageExtensions :: [Extension]
requiredLanguageExtensions =
  [ GADTs,
    TypeFamilies,
    TemplateHaskell,
    QuasiQuotes,
    FlexibleInstances
  ]

defaultMkEntityDecs :: [THEntityDef] -> Q [Dec]
defaultMkEntityDecs =
  fmap concat
    . mapM
      ( \def ->
          concat
            <$> mapM
              ($ def)
              [ mkEntityPhantomConstructors,
                mkEntityPhantomConstructorInstances,
                mkAutoKeyPersistFieldInstance,
                mkAutoKeyPrimitivePersistFieldInstance,
                mkEntityUniqueKeysPhantoms,
                mkUniqueKeysIsUniqueInstances,
                mkUniqueKeysEmbeddedInstances,
                mkUniqueKeysPersistFieldInstances,
                mkUniqueKeysPrimitiveOrPurePersistFieldInstances,
                mkKeyEqShowInstances,
                mkEntityPersistFieldInstance,
                mkEntitySinglePersistFieldInstance,
                mkPersistEntityInstance,
                mkEntityNeverNullInstance
              ]
      )

defaultMkEmbeddedDecs :: [THEmbeddedDef] -> Q [Dec]
defaultMkEmbeddedDecs =
  fmap concat
    . mapM
      ( \def ->
          concat
            <$> mapM
              ($ def)
              [ mkEmbeddedPersistFieldInstance,
                mkEmbeddedPurePersistFieldInstance,
                mkEmbeddedInstance
              ]
      )

defaultMkPrimitiveDecs :: [THPrimitiveDef] -> Q [Dec]
defaultMkPrimitiveDecs =
  fmap concat
    . mapM
      ( \def ->
          concat
            <$> mapM
              ($ def)
              [ mkPrimitivePersistFieldInstance,
                mkPrimitivePrimitivePersistFieldInstance
              ]
      )

#if MIN_VERSION_template_haskell(2, 17, 0)
fromDataD :: InstanceDec -> (Name, [TyVarBndr ()], [Con])
#else
fromDataD :: InstanceDec -> (Name, [TyVarBndr], [Con])
#endif
fromDataD dec = case dec of
#if MIN_VERSION_template_haskell(2, 11, 0)
  (DataD _ dName typeVars _ constrs _) -> (dName, typeVars, constrs)
#else
  (DataD _ dName typeVars constrs _) -> (dName, typeVars, constrs)
#endif
  _ -> error $ "Only datatypes can be processed: " ++ show dec
