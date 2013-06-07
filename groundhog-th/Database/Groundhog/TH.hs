{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

-- | This module provides functions to generate the auxiliary structures for the user data type
module Database.Groundhog.TH
  ( 
  -- * Settings format
  -- $settingsDoc
    mkPersist
  , groundhog
  , groundhogFile
  -- * Settings for code generation
  , CodegenConfig(..)
  , defaultCodegenConfig
  -- $namingStylesDoc
  , NamingStyle(..)
  , suffixNamingStyle
  , persistentNamingStyle
  , conciseNamingStyle
  ) where

import Database.Groundhog.Core (delim, UniqueType(..))
import Database.Groundhog.Generic
import Database.Groundhog.TH.CodeGen
import Database.Groundhog.TH.Settings
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (StrictType, VarStrictType, Lift(..))
import Language.Haskell.TH.Quote
import Control.Monad (forM, forM_, when, unless, liftM2)
import Data.ByteString.Char8 (pack)
import Data.Char (toUpper, toLower, isSpace)
import Data.Either (lefts)
import Data.List (nub, (\\))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Yaml as Y(decodeHelper, ParseException(..))
import qualified Text.Libyaml as Y

data CodegenConfig = CodegenConfig {
    -- | Naming style that is applied for all definitions
    namingStyle :: NamingStyle
    -- | Codegenerator will create a function with this name that will run 'migrate' for each non-polymorphic entity in definition
  , migrationFunction :: Maybe String
}

defaultCodegenConfig :: CodegenConfig
defaultCodegenConfig = CodegenConfig suffixNamingStyle Nothing

-- $namingStylesDoc
-- When describing a datatype you can omit the most of the declarations. 
-- In this case the omitted parts of description will be automatically generated using the default names created by naming style.
-- Any default name can be overridden by setting its value explicitly.

-- | Defines how the names are created. The mk* functions correspond to the set* functions.
-- Functions mkNormal* define names of non-record constructor Field
data NamingStyle = NamingStyle {
  -- | Create name of the table for the datatype. Parameters: data name.
    mkDbEntityName :: String -> String
  -- | Create name of the backend-specific key constructor for the datatype. Parameters: data name.
  , mkEntityKeyName :: String -> String
  -- | Create name for phantom constructor used to parametrise 'Field'. Parameters: data name, constructor name, constructor position.
  , mkPhantomName :: String -> String -> Int -> String
  -- | Create name for phantom unique key used to parametrise 'Key'. Parameters: data name, constructor name, unique constraint name.
  , mkUniqueKeyPhantomName :: String -> String -> String -> String
  -- | Create name of constructor for the unique key. Parameters: data name, constructor name, unique constraint name.
  , mkUniqueKeyConstrName :: String -> String -> String -> String
  -- | Create name used by 'persistName' for the unique key. Parameters: data name, constructor name, unique constraint name.
  , mkUniqueKeyDbName :: String -> String -> String -> String
  -- | Create name of the constructor specific table. Parameters: data name, constructor name, constructor position.
  , mkDbConstrName :: String -> String -> Int -> String
  -- | Create name of the db field for autokey. Parameters: data name, constructor name, constructor position.
  , mkDbConstrAutoKeyName :: String -> String -> Int -> String
  -- | Create name of the field column in a database. Parameters: data name, constructor name, constructor position, field record name, field position.
  , mkDbFieldName :: String -> String -> Int -> String -> Int -> String
  -- | Create name of field constructor used in expressions. Parameters: data name, constructor name, constructor position, field record name, field position.
  , mkExprFieldName :: String -> String -> Int -> String -> Int -> String
  -- | Create name of selector (see 'Embedded') constructor used in expressions. Parameters: data name, constructor name, field record name, field position.
  , mkExprSelectorName :: String -> String -> String -> Int -> String
  -- | Create field name used to refer to the it in settings for non-record constructors. Parameters: data name, constructor name, constructor position, field position.
  , mkNormalFieldName :: String -> String -> Int -> Int -> String
  -- | Create name of the field column in a database. Parameters: data name, constructor name, constructor position, field position.
  , mkNormalDbFieldName :: String -> String -> Int -> Int -> String
  -- | Create name of field constructor used in expressions. Parameters: data name, constructor name, constructor position, field position.
  , mkNormalExprFieldName :: String -> String -> Int -> Int -> String
  -- | Create name of selector (see 'Embedded') constructor used in expressions. Parameters: data name, constructor name, field position.
  , mkNormalExprSelectorName :: String -> String -> Int -> String
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
suffixNamingStyle = NamingStyle {
    mkDbEntityName = \dName -> dName
  , mkEntityKeyName = \dName -> dName ++ "Key"
  , mkPhantomName = \_ cName _ -> cName ++ "Constructor"
  , mkUniqueKeyPhantomName = \_ _ uName -> firstLetter toUpper uName
  , mkUniqueKeyConstrName = \_ _ uName -> firstLetter toUpper uName ++ "Key"
  , mkUniqueKeyDbName = \_ _ uName -> "Key" ++ [delim] ++ firstLetter toUpper uName
  , mkDbConstrName = \_ cName _ -> cName
  , mkDbConstrAutoKeyName = \_ _ _ -> "id"
  , mkDbFieldName = \_ _ _ fName _ -> fName
  , mkExprFieldName = \_ _ _ fName _ -> firstLetter toUpper fName ++ "Field"
  , mkExprSelectorName = \_ _ fName _ -> firstLetter toUpper fName ++ "Selector"
  , mkNormalFieldName = \_ cName _ fNum -> firstLetter toLower cName ++ show fNum
  , mkNormalDbFieldName = \_ cName _ fNum -> firstLetter toLower cName ++ show fNum
  , mkNormalExprFieldName = \_ cName _ fNum -> cName ++ show fNum ++ "Field"
  , mkNormalExprSelectorName = \_ cName fNum -> cName ++ show fNum ++ "Selector"
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
persistentNamingStyle = suffixNamingStyle {
    mkExprFieldName = \_ cName _ fName _ -> cName ++ firstLetter toUpper fName
  , mkExprSelectorName = \_ cName fName _ -> cName ++ firstLetter toUpper fName
  , mkNormalExprFieldName = \_ cName _ fNum -> cName ++ show fNum
  , mkNormalExprSelectorName = \_ cName fNum -> cName ++ show fNum
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
conciseNamingStyle = suffixNamingStyle {
    mkExprFieldName = \_ _ _ fName _ -> firstLetter toUpper fName
  , mkExprSelectorName = \_ _ fName _ -> firstLetter toUpper fName
  , mkNormalExprFieldName = \_ cName _ fNum -> cName ++ show fNum
  , mkNormalExprSelectorName = \_ cName fNum -> cName ++ show fNum
}

-- | Creates the auxiliary structures. 
-- Particularly, it creates GADT 'Field' data instance for referring to the fields in expressions and phantom types for data constructors.
-- The default names of auxiliary datatypes and names used in database are generated using the naming style and can be changed via configuration.
-- The datatypes and their generation options are defined via YAML configuration parsed by quasiquoter 'groundhog'. 
mkPersist :: CodegenConfig -> PersistDefinitions -> Q [Dec]
mkPersist CodegenConfig{..} (PersistDefinitions defs) = do
  let duplicates = notUniqueBy id $ map (either psDataName psEmbeddedName) defs
  unless (null duplicates) $ fail $ "All definitions must be unique. Found duplicates: " ++ show duplicates
  defs' <- forM defs $ \def -> do
    let name = mkName $ either psDataName psEmbeddedName def
    info <- reify name
    return $ case info of
      TyConI x -> case x of
        d@DataD{}  -> case def of
          Left  ent -> either error Left $ validateEntity $ applyEntitySettings namingStyle ent $ mkTHEntityDefWith namingStyle d
          Right emb -> either error Right $ validateEmbedded $ applyEmbeddedSettings emb $ mkTHEmbeddedDefWith namingStyle d
        NewtypeD{} -> error "Newtypes are not supported"
        _ -> error $ "Unknown declaration type: " ++ show name ++ " " ++ show x
      _        -> error $ "Only datatypes can be processed: " ++ show name
  decs <- mapM (either mkEntityDecs mkEmbeddedDecs) defs'
  migrateFunc <- maybe (return []) (\name -> mkMigrateFunction name (lefts defs')) migrationFunction
  return $ migrateFunc ++ concat decs

applyEntitySettings :: NamingStyle -> PSEntityDef -> THEntityDef -> THEntityDef
applyEntitySettings style PSEntityDef{..} def@(THEntityDef{..}) =
  def { thDbEntityName = fromMaybe thDbEntityName psDbEntityName
      , thEntitySchema = psEntitySchema
      , thAutoKey = thAutoKey'
      , thUniqueKeys = maybe thUniqueKeys (map mkUniqueKey') psUniqueKeys
      , thConstructors = thConstructors'
      } where
  thAutoKey' = maybe thAutoKey (liftM2 applyAutoKeySettings thAutoKey) psAutoKey

  thConstructors' = maybe thConstructors'' (f thConstructors'') $ psConstructors where
    thConstructors'' = maybe id (\_ -> zipWith putAutoKey [0..]) thAutoKey' thConstructors
    putAutoKey cNum cDef@(THConstructorDef{..}) = cDef {thDbAutoKeyName = Just $ mkDbConstrAutoKeyName style (nameBase thDataName) (nameBase thConstrName) cNum}

  mkUniqueKey' = mkUniqueKey style (nameBase thDataName) (head thConstructors')
  f = foldr $ replaceOne "constructor" psConstrName (nameBase . thConstrName) applyConstructorSettings

mkUniqueKey :: NamingStyle -> String -> THConstructorDef -> PSUniqueKeyDef -> THUniqueKeyDef
mkUniqueKey style@NamingStyle{..} dName cDef@THConstructorDef{..} PSUniqueKeyDef{..} = key where
  key = THUniqueKeyDef {
    thUniqueKeyName = psUniqueKeyName
  , thUniqueKeyPhantomName = fromMaybe (mkUniqueKeyPhantomName dName (nameBase thConstrName) psUniqueKeyName) psUniqueKeyPhantomName
  , thUniqueKeyConstrName = fromMaybe (mkUniqueKeyConstrName dName (nameBase thConstrName) psUniqueKeyName) psUniqueKeyConstrName
  , thUniqueKeyDbName = fromMaybe (mkUniqueKeyDbName dName (nameBase thConstrName) psUniqueKeyName) psUniqueKeyDbName
  , thUniqueKeyFields = maybe uniqueFields (f uniqueFields) psUniqueKeyFields
  , thUniqueKeyMakeEmbedded = fromMaybe False psUniqueKeyMakeEmbedded
  , thUniqueKeyIsDef = fromMaybe False psUniqueKeyIsDef
  }
  f = foldr $ replaceOne "unique field" psFieldName thFieldName applyFieldSettings
  uniqueFields = mkFieldsForUniqueKey style dName key cDef

applyAutoKeySettings :: THAutoKeyDef -> PSAutoKeyDef -> THAutoKeyDef
applyAutoKeySettings def@(THAutoKeyDef{..}) PSAutoKeyDef{..} = 
  def { thAutoKeyConstrName = fromMaybe thAutoKeyConstrName psAutoKeyConstrName
      , thAutoKeyIsDef = fromMaybe thAutoKeyIsDef psAutoKeyIsDef
      }

applyConstructorSettings :: PSConstructorDef -> THConstructorDef -> THConstructorDef
applyConstructorSettings PSConstructorDef{..} def@(THConstructorDef{..}) =
  def { thPhantomConstrName = fromMaybe thPhantomConstrName psPhantomConstrName
      , thDbConstrName = fromMaybe thDbConstrName psDbConstrName
      , thDbAutoKeyName = fromMaybe thDbAutoKeyName psDbAutoKeyName
      , thConstrFields = maybe thConstrFields (f thConstrFields) psConstrFields
      , thConstrUniques = maybe thConstrUniques (map convertUnique) psConstrUniques
      } where
  f = foldr $ replaceOne "field" psFieldName thFieldName applyFieldSettings
  convertUnique (PSUniqueDef uName uType uFields) = THUniqueDef uName (fromMaybe UniqueConstraint uType) uFields
  
applyFieldSettings :: PSFieldDef -> THFieldDef -> THFieldDef
applyFieldSettings PSFieldDef{..} def@(THFieldDef{..}) =
  def { thDbFieldName = fromMaybe thDbFieldName psDbFieldName
      , thExprName = fromMaybe thExprName psExprName
      , thDbTypeName = psDbTypeName
      , thEmbeddedDef = psEmbeddedDef
      , thFieldOnDelete = psFieldOnDelete
      , thFieldOnUpdate = psFieldOnUpdate
      }

applyEmbeddedSettings :: PSEmbeddedDef -> THEmbeddedDef -> THEmbeddedDef
applyEmbeddedSettings PSEmbeddedDef{..} def@(THEmbeddedDef{..}) =
  def { thDbEmbeddedName = fromMaybe thDbEmbeddedName psDbEmbeddedName
      , thEmbeddedFields = maybe thEmbeddedFields (f thEmbeddedFields) psEmbeddedFields
      } where
  f = foldr $ replaceOne "field" psFieldName thFieldName applyFieldSettings

mkFieldsForUniqueKey :: NamingStyle -> String -> THUniqueKeyDef -> THConstructorDef -> [THFieldDef]
mkFieldsForUniqueKey style dName uniqueKey cDef = zipWith (setSelector . findField) (thUniqueFields uniqueDef) [0..] where
  findField name = findOne "field" id thFieldName name $ thConstrFields cDef
  uniqueDef = findOne "unique" id thUniqueName (thUniqueKeyName uniqueKey) $ thConstrUniques cDef
  setSelector f i = f {thExprName = mkExprSelectorName style dName (thUniqueKeyConstrName uniqueKey) (thFieldName f) i}

notUniqueBy :: Eq b => (a -> b) -> [a] -> [b]
notUniqueBy f xs = let xs' = map f xs in nub $ xs' \\ nub xs'

assertUnique :: (Monad m, Eq b, Show b) => (a -> b) -> [a] -> String -> m ()
assertUnique f xs what = case notUniqueBy f xs of
  [] -> return ()
  ys -> fail $ "All " ++ what ++ " must be unique: " ++ show ys

-- we need to validate datatype names because TH just creates unusable fields with spaces
assertSpaceFree :: Monad m => String -> String -> m ()
assertSpaceFree s what = when (any isSpace s) $ fail $ "Spaces in " ++ what ++ " are not allowed: " ++ show s

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
      ys -> fail $ "Constraints must have at least one field: " ++ show ys
    when (isNothing (thDbAutoKeyName cdef) /= isNothing (thAutoKey def)) $
      fail $ "Presence of autokey definitions should be the same in entity and constructors definitions " ++ show (thDataName def)
      
  -- check that unique keys = [] for multiple constructor datatype
  if length constrs > 1 && not (null $ thUniqueKeys def)
    then fail $ "Unique keys may exist only for datatypes with single constructor: " ++ show (thDataName def)
    else -- check that all unique keys reference existing uniques
         let uniqueNames = map thUniqueName $ thConstrUniques $ head constrs
         in  forM_ (thUniqueKeys def) $ \cKey -> unless (thUniqueKeyName cKey `elem` uniqueNames) $
             fail $ "Unique key mentions unknown unique: " ++ thUniqueKeyName cKey ++ " in datatype " ++ show (thDataName def)
  let primaryConstraints = length $ filter ((== UniquePrimary) . thUniqueType) $ concatMap thConstrUniques constrs 
  if length constrs > 1
    then when (primaryConstraints > 0) $
           fail $ "Custom primary keys may exist only for datatypes with single constructor: " ++ show (thDataName def)
    else when (primaryConstraints + maybe 0 (const 1) (thAutoKey def) > 1) $
           fail $ "A datatype may have either an auto key or one custom primary key constraint : " ++ show (thDataName def)
  -- check that if unique keys = [] there is auto key
  when (null (thUniqueKeys def) && isNothing (thAutoKey def)) $
    fail $ "A datatype must have either an auto key or unique keys: " ++ show (thDataName def)
  -- check that only one of the keys is default
  let defaults = maybe False thAutoKeyIsDef (thAutoKey def) : map thUniqueKeyIsDef (thUniqueKeys def)
  when (length (filter id defaults) /= 1) $
    fail $ "A datatype must have exactly one default key: " ++ show (thDataName def)
  return def
  
validateField :: THFieldDef -> Either String ()
validateField fDef = do
  assertSpaceFree (thExprName fDef) "field expr name"
  when (isJust (thDbTypeName fDef) && isJust (thEmbeddedDef fDef)) $
    fail $ "A field may not have both type and embeddedType: " ++ show (thFieldName fDef)

validateEmbedded :: THEmbeddedDef -> Either String THEmbeddedDef
validateEmbedded def = do
  let fields = thEmbeddedFields def
  assertUnique thExprName fields "expr field name in an embedded datatype"
  assertUnique thDbFieldName fields "db field name in an embedded datatype"
  mapM_ validateField fields
  return def

mkTHEntityDefWith :: NamingStyle -> Dec -> THEntityDef
mkTHEntityDefWith NamingStyle{..} (DataD _ dName typeVars cons _) =
  THEntityDef dName (mkDbEntityName dName') Nothing (Just $ THAutoKeyDef (mkEntityKeyName dName') True) [] typeVars constrs where
  constrs = zipWith mkConstr [0..] cons
  dName' = nameBase dName

  mkConstr cNum c = case c of
    NormalC name params -> mkConstr' name $ zipWith (mkField (nameBase name)) params [0..]
    RecC name params -> mkConstr' name $ zipWith (mkVarField (nameBase name)) params [0..]
    InfixC{} -> error $ "Types with infix constructors are not supported" ++ show dName
    ForallC{} -> error $ "Types with existential quantification are not supported" ++ show dName
   where
    mkConstr' name params = THConstructorDef name (apply mkPhantomName) (apply mkDbConstrName) Nothing params [] where
      apply f = f dName' (nameBase name) cNum

    mkField :: String -> StrictType -> Int -> THFieldDef
    mkField cName (_, t) fNum = THFieldDef (apply mkNormalFieldName) (apply mkNormalDbFieldName) Nothing (apply mkNormalExprFieldName) t Nothing Nothing Nothing where
      apply f = f dName' cName cNum fNum
    mkVarField :: String -> VarStrictType -> Int -> THFieldDef
    mkVarField cName (fName, _, t) fNum = THFieldDef fName' (apply mkDbFieldName) Nothing (apply mkExprFieldName) t Nothing Nothing Nothing where
      apply f = f dName' cName cNum fName' fNum
      fName' = nameBase fName
mkTHEntityDefWith _ _ = error "Only datatypes can be processed"

mkTHEmbeddedDefWith :: NamingStyle -> Dec -> THEmbeddedDef
mkTHEmbeddedDefWith (NamingStyle{..}) (DataD _ dName typeVars cons _) =
  THEmbeddedDef dName cName (mkDbEntityName dName') typeVars fields where
  dName' = nameBase dName
  
  (cName, fields) = case cons of
    [cons'] -> case cons' of
      NormalC name params -> (name, zipWith (mkField (nameBase name)) params [0..])
      RecC name params -> (name, zipWith (mkVarField (nameBase name)) params [0..])
      InfixC{} -> error $ "Types with infix constructors are not supported" ++ show dName
      ForallC{} -> error $ "Types with existential quantification are not supported" ++ show dName
    _ -> error $ "An embedded datatype must have exactly one constructor: " ++ show dName
  
  mkField :: String -> StrictType -> Int -> THFieldDef
  mkField cName' (_, t) fNum = THFieldDef (apply mkNormalFieldName) (apply mkNormalDbFieldName) Nothing (mkNormalExprSelectorName dName' cName' fNum) t Nothing Nothing Nothing where
    apply f = f dName' cName' 0 fNum
  mkVarField :: String -> VarStrictType -> Int -> THFieldDef
  mkVarField cName' (fName, _, t) fNum = THFieldDef fName' (apply mkDbFieldName) Nothing (mkExprSelectorName dName' cName' fName' fNum) t Nothing Nothing Nothing where
    apply f = f dName' cName' 0 fName' fNum
    fName' = nameBase fName
mkTHEmbeddedDefWith _ _ = error "Only datatypes can be processed"

firstLetter :: (Char -> Char) -> String -> String
firstLetter f s = f (head s):tail s

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
--data Settable = First {foo :: String, bar :: Int, next :: Maybe (Key Settable BackendSpecific)} deriving (Eq, Show)
--
--    \-- The declaration with defaulted names
--
--mkPersist defaultCodegenConfig [groundhog|
--entity: Settable                       # If we did not want to add a constraint, this line would be enough
--keys:
--  - name: someconstraint
--constructors:
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
--mkPersist defaultCodegenConfig [groundhog|
--definitions:                           # First level key whose value is a list of definitions. It can be considered an optional header.
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
--            \# type: varchar            # This would result in having field type DbOther \"varchar\" instead of DbString. Value of this attribute will be used by DB backend for migration
--          - name: bar
--            dbName: bar
--            exprName: BarField
--                                       # For some databases \"type: integer\" would be appropriate
--          - name: next
--            dbName: next
--            exprName: NextField
--            \# If these clauses are omitted, the database will define action automatically. Note that it may differ across databases. For example, MySQL has \"restrict\" by default, but in PostgreSQL it is \"no action\"
--            \# onDelete: cascade        # Defines ON DELETE clause of references. It can have values: no action, restrict, cascade, set null, set default
--            \# onUpdate: set null       # Defines ON UPDATE
--        uniques:
--          - name: someconstraint
--            type: constraint           # The type can be be \"constraint\", \"index\", or \"primary\"
--            fields: [foo, bar]         # List of constructor parameter names. Not DB names(!)
-- |]
-- @
--
-- This is an example of embedded datatype usage.
--
-- @
--data Company = Company {name :: String, headquarter :: Address, dataCentre :: Address, salesOffice :: Address} deriving (Eq, Show)
--data Address = Address {city :: String, zipCode :: String, street :: String} deriving (Eq, Show)
--
--mkPersist defaultCodegenConfig [groundhog|
--definitions:
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

-- | Converts quasiquoted settings into the datatype used by mkPersist.
groundhog :: QuasiQuoter
groundhog = QuasiQuoter { quoteExp  = parseDefinitions
                        , quotePat  = error "groundhog: pattern quasiquoter"
                        , quoteType = error "groundhog: type quasiquoter"
                        , quoteDec  = error "groundhog: declaration quasiquoter"
                        }

-- | Parses configuration stored in the file
--
-- > mkPersist suffixNamingStyle [groundhogFile|../groundhog.yaml|]
groundhogFile :: QuasiQuoter
groundhogFile = quoteFile groundhog

parseDefinitions :: String -> Q Exp
parseDefinitions s = do
  result <- runIO $ decodeHelper (Y.decode $ pack s)
  case result of
    Left err -> case err of
      InvalidYaml (Just (Y.YamlParseException problem context mark)) -> fail $ unlines
        [ "YAML parse error: " ++ problem
        , "Context: " ++ context
        , "At line: " ++ show (Y.yamlLine mark)
        , lines s !! Y.yamlLine mark
        , replicate (Y.yamlColumn mark) ' ' ++ "^"
        ]
      _ -> fail $ show err
    Right (Left err) -> fail err
    Right (Right result') -> lift (result' :: PersistDefinitions)

mkEntityDecs :: THEntityDef -> Q [Dec]
mkEntityDecs def = do
  --runIO (print def)
  decs <- fmap concat $ sequence
    [ mkEntityPhantomConstructors def
    , mkEntityPhantomConstructorInstances def
    , mkAutoKeyPersistFieldInstance def
    , mkAutoKeyPrimitivePersistFieldInstance def
    , mkEntityUniqueKeysPhantoms def
    , mkUniqueKeysIsUniqueInstances def
    , mkUniqueKeysEmbeddedInstances def
    , mkUniqueKeysPersistFieldInstances def
    , mkUniqueKeysPrimitiveOrPurePersistFieldInstances def
    , mkKeyEqShowInstances def
    , mkEntityPersistFieldInstance def
    , mkEntitySinglePersistFieldInstance def
    , mkPersistEntityInstance def
    , mkEntityNeverNullInstance def
    ]
  --runIO $ putStrLn $ pprint decs
  return decs

mkEmbeddedDecs :: THEmbeddedDef -> Q [Dec]
mkEmbeddedDecs def = do
  --runIO (print def)
  decs <- fmap concat $ sequence
    [ mkEmbeddedPersistFieldInstance def
    , mkEmbeddedPurePersistFieldInstance def
    , mkEmbeddedInstance def
    ]
--  runIO $ putStrLn $ pprint decs
  return decs
