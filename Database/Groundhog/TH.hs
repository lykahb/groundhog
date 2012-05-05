{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

-- | This module provides functions to generate the auxiliary structures for the user data type
module Database.Groundhog.TH
  ( 
  -- * Settings format
  -- $settingsDoc
    mkPersist
  , groundhog
  , groundhogFile
  -- * Naming styles
  -- $namingStylesDoc
  , NamingStyle(..)
  , suffixNamingStyle
  , persistentNamingStyle
  , conciseNamingStyle
  ) where

import Database.Groundhog.TH.CodeGen
import Database.Groundhog.TH.Settings
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (StrictType, VarStrictType, Lift(..))
import Language.Haskell.TH.Quote
import Control.Monad (forM, forM_, when)
import Data.ByteString.Char8 (pack)
import Data.Char (toUpper, toLower, isSpace)
import Data.List (nub, (\\))
import Data.Yaml (decodeEither)

-- $namingStylesDoc
-- When describing a datatype you can omit the most of the declarations. 
-- In this case the omitted parts of description will be automatically generated using the default names created by naming style.
-- Any default name can be overridden by setting its value explicitly.

-- | Defines how the names are created. The mk* functions correspond to the set* functions.
-- Functions mkNormal* define names of non-record constructor Field
data NamingStyle = NamingStyle {
  -- | Create name of the table for the datatype. Parameters: data name.
    mkDbEntityName :: String -> String
  -- | Create name for phantom constructor used to parametrise 'Field'. Parameters: data name, constructor name, constructor position.
  , mkPhantomName :: String -> String -> Int -> String
  -- | Create name of the constructor specific table. Parameters: data name, constructor name, constructor position.
  , mkDbConstrName :: String -> String -> Int -> String
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
  , mkPhantomName = \_ cName _ -> cName ++ "Constructor"
  , mkDbConstrName = \_ cName _ -> cName
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

replaceOne :: (Eq c, Show c) => (a -> c) -> (b -> c) -> (a -> b -> b) -> a -> [b] -> [b]
replaceOne getter1 getter2 apply a bs = case length (filter ((getter1 a ==) . getter2) bs) of
  1 -> map (\b -> if getter1 a == getter2 b then apply a b else b) bs
  0 -> error $ "Element with name " ++ show (getter1 a) ++ " not found"
  _ -> error $ "Found more than one element with name " ++ show (getter1 a)

-- | Creates the auxiliary structures. 
-- Particularly, it creates GADT 'Field' data instance for referring to the fields in expressions and phantom types for data constructors.
-- The default names of auxiliary datatypes and names used in database are generated using the naming style and can be changed via configuration.
-- The datatypes and their generation options are defined via YAML configuration parsed by quasiquoter 'groundhog'. 
mkPersist :: NamingStyle -> PersistSettings -> Q [Dec]
mkPersist style (PersistSettings defs) = do
  let duplicates = notUniqueBy id $ map (either psDataName psEmbeddedName) defs
  when (not $ null duplicates) $ fail $ "All definitions must be unique. Found duplicates: " ++ show duplicates
  decs <- forM defs $ \def -> do
    let name = mkName $ either psDataName psEmbeddedName def
    info <- reify name
    case info of
      TyConI x -> do
        case x of
          d@(DataD _ _ _ _ _)  -> case def of
            Left  ent -> mkEntityDecs $ either error id $ validateEntity $ applyEntitySettings ent $ mkTHEntityDefWith style d
            Right emb -> mkEmbeddedDecs $ either error id $ validateEmbedded $ applyEmbeddedSettings emb $ mkTHEmbeddedDefWith style d
          NewtypeD _ _ _ _ _ -> error "Newtypes are not supported"
          _ -> error $ "Unknown declaration type: " ++ show name ++ " " ++ show x
      _        -> error $ "Only datatypes can be processed: " ++ show name
  return $ concat decs

applyEntitySettings :: PSEntityDef -> THEntityDef -> THEntityDef
applyEntitySettings settings def@(THEntityDef{..}) =
  def { dbEntityName = maybe dbEntityName id $ psDbEntityName settings
      , thConstructors = maybe thConstructors (f thConstructors) $ psConstructors settings
      } where
  f = foldr $ replaceOne psConstrName (nameBase . thConstrName) applyConstructorSettings

applyConstructorSettings :: PSConstructorDef -> THConstructorDef -> THConstructorDef
applyConstructorSettings settings def@(THConstructorDef{..}) =
  def { thPhantomConstrName = maybe thPhantomConstrName id $ psPhantomConstrName settings
      , dbConstrName = maybe dbConstrName id $ psDbConstrName settings
      , thConstrFields = maybe thConstrFields (f thConstrFields) $ psConstrFields settings
      , thConstrConstrs = maybe thConstrConstrs id $ psConstrConstrs settings
      } where
  f = foldr $ replaceOne psFieldName fieldName applyFieldSettings
  
applyFieldSettings :: PSFieldDef -> THFieldDef -> THFieldDef
applyFieldSettings settings def@(THFieldDef{..}) =
  def { dbFieldName = maybe dbFieldName id $ psDbFieldName settings
      , exprName = maybe exprName id $ psExprName settings
      , embeddedDef = psEmbeddedDef settings
      }

applyEmbeddedSettings :: PSEmbeddedDef -> THEmbeddedDef -> THEmbeddedDef
applyEmbeddedSettings settings def@(THEmbeddedDef{..}) =
  def { dbEmbeddedName = maybe dbEmbeddedName id $ psDbEmbeddedName settings
      , embeddedFields = maybe embeddedFields (f embeddedFields) $ psEmbeddedFields settings
      } where
  f = foldr $ replaceOne psFieldName fieldName applyFieldSettings

notUniqueBy :: Eq b => (a -> b) -> [a] -> [b]
notUniqueBy f xs = let xs' = map f xs in nub $ xs' \\ nub xs'

assertUnique :: (Monad m, Eq b, Show b) => (a -> b) -> [a] -> String -> m ()
assertUnique f xs what = case notUniqueBy f xs of
        [] -> return ()
        ys -> fail $ "All " ++ what ++ " must be unique: " ++ show ys

-- we need to validate datatype names because TH just creates unusable fields with spaces
assertSpaceFree :: Monad m => String -> String -> m ()
assertSpaceFree s what = if all (not . isSpace) s then return () else fail $ "Spaces in " ++ what ++ " are not allowed: " ++ show s

validateEntity :: THEntityDef -> Either String THEntityDef
validateEntity def = do
  let constrs = thConstructors def
  assertUnique thPhantomConstrName constrs "constructor phantom name"
  assertUnique dbConstrName constrs "constructor db name"
  forM_ constrs $ \cdef -> do
    let fields = thConstrFields cdef
    assertSpaceFree (thPhantomConstrName cdef) "constructor phantom name"
    assertUnique exprName fields "expr field name in a constructor"
    assertUnique dbFieldName fields "db field name in a constructor"
    forM_ fields $ \fdef -> assertSpaceFree (exprName fdef) "field expr name"
    case filter (\(PSConstraintDef _ cfields) -> null cfields) $ thConstrConstrs cdef of
      [] -> return ()
      ys -> fail $ "Constraints cannot have no fields: " ++ show ys
  return def

validateEmbedded :: THEmbeddedDef -> Either String THEmbeddedDef
validateEmbedded def = do
  let fields = embeddedFields def
  assertUnique exprName fields "expr field name in an embedded datatype"
  assertUnique dbFieldName fields "db field name in an embedded datatype"
  forM_ fields $ \fdef -> assertSpaceFree (exprName fdef) "field expr name"
  return def

mkTHEntityDefWith :: NamingStyle -> Dec -> THEntityDef
mkTHEntityDefWith (NamingStyle{..}) (DataD _ dName typeVars cons _) =
  THEntityDef dName (mkDbEntityName dName') typeVars constrs where
  constrs = zipWith mkConstr [0..] cons
  dName' = nameBase dName

  mkConstr cNum c = case c of
    (NormalC name params) -> mkConstr' name $ zipWith (mkField (nameBase name)) params [0..]
    (RecC name params) -> mkConstr' name $ zipWith (mkVarField (nameBase name)) params [0..]
    (InfixC _ _ _) -> error $ "Types with infix constructors are not supported" ++ show dName
    (ForallC _ _ _) -> error $ "Types with existential quantification are not supported" ++ show dName
   where
    mkConstr' name params = THConstructorDef name (mkPhantomName dName' (nameBase name) cNum) (mkDbConstrName dName' (nameBase name) cNum) params []

    mkField :: String -> StrictType -> Int -> THFieldDef
    mkField cName (_, t) fNum = THFieldDef (apply mkNormalFieldName) (apply mkNormalDbFieldName) (apply mkNormalExprFieldName) t Nothing where
      apply f = f dName' cName cNum fNum
    mkVarField :: String -> VarStrictType -> Int -> THFieldDef
    mkVarField cName (fName, _, t) fNum = THFieldDef fName' (apply mkDbFieldName) (apply mkExprFieldName) t Nothing where
      apply f = f dName' cName cNum fName' fNum
      fName' = nameBase fName
mkTHEntityDefWith _ _ = error "Only datatypes can be processed"

mkTHEmbeddedDefWith :: NamingStyle -> Dec -> THEmbeddedDef
mkTHEmbeddedDefWith (NamingStyle{..}) (DataD _ dName typeVars cons _) =
  THEmbeddedDef dName cName (mkDbEntityName dName') typeVars fields where
  dName' = nameBase dName
  
  (cName, fields) = case cons of
    [NormalC name params] -> (name, zipWith (mkField (nameBase name)) params [0..])
    [RecC name params] -> (name, zipWith (mkVarField (nameBase name)) params [0..])
    [InfixC _ _ _] -> error $ "Types with infix constructors are not supported" ++ show dName
    [ForallC _ _ _] -> error $ "Types with existential quantification are not supported" ++ show dName
    _ -> error $ "An embedded datatype must have exactly one constructor: " ++ show dName
  
  mkField :: String -> StrictType -> Int -> THFieldDef
  mkField cName' (_, t) fNum = THFieldDef (apply mkNormalFieldName) (apply mkNormalDbFieldName) (mkNormalExprSelectorName dName' cName' fNum) t Nothing where
    apply f = f dName' cName' 0 fNum
  mkVarField :: String -> VarStrictType -> Int -> THFieldDef
  mkVarField cName' (fName, _, t) fNum = THFieldDef fName' (apply mkDbFieldName) (mkExprSelectorName dName' cName' fName' fNum) t Nothing where
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
-- The example below has all properties set explicitly.
--
-- @
--data Settable = First {foo :: String, bar :: Int} deriving (Eq, Show)
--
--mkPersist suffixNamingStyle [groundhog|
--definitions:                           # First level key whose value is a list of definitions. It can be considered an optional header.
--                                       # The list elements start with hyphen+space. Keys are separated from values by a colon+space.
--  - entity: Settable                   # Mandatory. Entity datatype name
--    dbName: Settable                   # Name of the main table
--    constructors:                      # List of constructors. The constructors you don't change can be omitted
--      - name: First                    # Mandatory. Constructor name
--        phantomName: FooBarConstructor # Constructor phantom type name used to guarantee type safety
--        dbName: First                  # Name of constructor table which is created only for datatypes with multiple constructors
--        fields:                        # List of constructor fields. If you don't change a field, you can omit it
--          - name: foo
--            dbName: foo                # Column name
--            exprName: FooField         # Name of a field used in expressions
--          - name: bar
--            dbName: bar
--            exprName: BarField
--        constraints:
--          - name: someconstraint
--            fields: [foo, bar]         # List of constructor parameter names. Not DB names(!)
-- |]
-- @
--
-- which is equivalent to the declaration with defaulted names
--
-- @
--mkPersist suffixNamingStyle [groundhog|
--entity: Settable                       # If we did not want to add a constraint, this line would be enough
--constructors:
--  - name: First
--    constraints:
--      - name: someconstraint
--        fields: [foo, bar]
-- |]
-- @
--
-- This is an example of embedded datatype usage.
--
-- @
--data Company = Company {name :: String, headquarter :: Address, dataCentre :: Address, salesOffice :: Address} deriving (Eq, Show)
--data Address = Address {city :: String, zipCode :: String, street :: String} deriving (Eq, Show)
--
--mkPersist suffixNamingStyle [groundhog|
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
groundhog = QuasiQuoter { quoteExp  = parseSettings
                        , quotePat  = error "groundhog: pattern quasiquoter"
                        , quoteType = error "groundhog: type quasiquoter"
                        , quoteDec  = error "groundhog: declaration quasiquoter"
                        }

-- | Parses configuration stored in the file
--
-- > mkPersist suffixNamingStyle [groundhogFile|../groundhog.yaml|]
groundhogFile :: QuasiQuoter
groundhogFile = quoteFile groundhog

parseSettings :: String -> Q Exp
parseSettings s = either fail lift result where
  result = decodeEither $ pack s :: Either String PersistSettings


mkEntityDecs :: THEntityDef -> Q [Dec]
mkEntityDecs def = do
  --runIO (print def)
  decs <- fmap concat $ sequence
    [ mkEntityPhantomConstructors def
    , mkEntityPhantomConstructorInstances def
    , mkEntityPersistFieldInstance def
    , mkEntitySinglePersistFieldInstance def
    , mkPersistEntityInstance def
--    , mkEntityNeverNullInstance def
    ]
--  runIO $ putStrLn $ pprint decs
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
