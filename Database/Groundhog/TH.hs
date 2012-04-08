{-# LANGUAGE TemplateHaskell, RecordWildCards, DoAndIfThenElse #-}
{-# LANGUAGE CPP #-}

-- | This module provides functions to generate the auxiliary structures for the user data type
module Database.Groundhog.TH
  ( mkPersist
  , groundhog
  , groundhogFile
  , NamingStyle(..)
  , fieldNamingStyle
  , persistentNamingStyle
  , conciseNamingStyle
  ) where

import Database.Groundhog.Core (PersistEntity(..), Key, PersistField(..), SinglePersistField(..), PrimitivePersistField(..), PersistBackend(..), DbType(..), Constraint(..), Constructor(..), EntityDef(..), ConstructorDef(..), PersistValue(..), NeverNull)
import Database.Groundhog.Generic (failMessage, applyEmbeddedSettings)
import Database.Groundhog.TH.Settings
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (StrictType, VarStrictType, Lift(..))
import Language.Haskell.TH.Quote
import Control.Monad (liftM, forM, forM_, foldM, when)
import Data.ByteString.Char8 (pack)
import Data.Char (toUpper, toLower, isSpace)
import Data.Either ( partitionEithers)
import Data.List (nub, (\\))
import Data.Yaml (decodeEither)

-- data SomeData a = U1 { foo :: Int} | U2 { bar :: Maybe String, asc :: Int64, add :: a} | U3 deriving (Show, Eq)

data THEntityDef = THEntityDef {
    dataName :: Name -- SomeData
  , dbEntityName :: String  -- SQLSomeData
  , thTypeParams :: [TyVarBndr]
  , thConstructors :: [THConstructorDef]
} deriving Show

data THEmbeddedDef = THEmbeddedDef {
    embeddedName :: Name
  , dbEmbeddedName :: String -- used only to set polymorphic part of name of its container
  , thEmbeddedTypeParams :: [TyVarBndr]
  , embeddedFields :: [THFieldDef]
} deriving Show

data THConstructorDef = THConstructorDef {
    thConstrName    :: Name -- U2
  , thPhantomConstrName :: String -- U2Constructor
  , dbConstrName    :: String -- SQLU2
  , thConstrParams  :: [THFieldDef]
  , thConstrConstrs :: [PSConstraintDef]
} deriving Show

data THFieldDef = THFieldDef {
    fieldName :: String -- bar
  , dbFieldName :: String -- SQLbar
  , exprName :: String -- BarField
  , fieldType :: Type
  , embeddedDef :: Maybe [PSEmbeddedFieldDef]
} deriving Show

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
  -- | Create field name used to refer to the it. Parameters: data name, constructor name, constructor position, field position.
  , mkNormalFieldName :: String -> String -> Int -> Int -> String
  -- | Create name of the field column in a database. Parameters: data name, constructor name, constructor position, field position.
  , mkNormalDbFieldName :: String -> String -> Int -> Int -> String
  -- | Create name of field constructor used in expressions. Parameters: data name, constructor name, constructor position, field position.
  , mkNormalExprFieldName :: String -> String -> Int -> Int -> String
}

-- | Default style. Adds \"Field\" to each record field name
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
fieldNamingStyle :: NamingStyle
fieldNamingStyle = NamingStyle {
    mkDbEntityName = \dName -> dName
  , mkPhantomName = \_ cName _ -> cName ++ "Constructor"
  , mkDbConstrName = \_ cName _ -> cName
  , mkDbFieldName = \_ _ _ fName _ -> fName
  , mkExprFieldName = \_ _ _ fName _ -> firstLetter toUpper fName ++ "Field"
  , mkNormalFieldName = \_ cName _ fNum -> firstLetter toLower cName ++ show fNum
  , mkNormalDbFieldName = \_ cName _ fNum -> firstLetter toLower cName ++ show fNum
  , mkNormalExprFieldName = \_ cName _ fNum -> cName ++ show fNum
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
persistentNamingStyle = fieldNamingStyle {
  mkExprFieldName = \_ cName _ fName _ -> cName ++ firstLetter toUpper fName
}

-- | Creates the shortest field names. Very prone to conflicts
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
conciseNamingStyle = fieldNamingStyle {
  mkExprFieldName = \_ _ _ fName _ -> firstLetter toUpper fName
}

replaceOne :: (Eq c, Show c) => (a -> c) -> (b -> c) -> (a -> b -> b) -> a -> [b] -> [b]
replaceOne getter1 getter2 apply a bs = case length (filter ((getter1 a ==) . getter2) bs) of
  1 -> map (\b -> if getter1 a == getter2 b then apply a b else b) bs
  0 -> error $ "Element with name " ++ show (getter1 a) ++ " not found"
  _ -> error $ "Found more than one element with name " ++ show (getter1 a)

-- | Create the auxiliary structures. 
-- Particularly, it creates GADT 'Field' data instance for referring to the fields in expressions and phantom types for data constructors.
-- The default names of auxiliary datatypes and names used in database are generated using the naming style and can be changed via configuration.
-- The datatypes and their generation options are defined via YAML configuration parsed by quasiquoter 'groundhog'. 
mkPersist :: NamingStyle -> PersistSettings -> Q [Dec]
mkPersist style (PersistSettings defs) = do
  let duplicates = notUniqueBy id $ map (either psDataName psEmbeddedName) defs
  when (not $ null duplicates) $ fail $ "All definitions must be unique. Found duplicates: " ++ show duplicates
  let (entities, embeddeds) = partitionEithers defs
  entitiesDecs <- forM entities $ \ent -> do
    let name = mkName $ psDataName ent
    info <- reify name
    case info of
      TyConI x -> do
        case x of
          def@(DataD _ _ _ _ _)  -> mkDecs $ either error id $ validate $ applyEntitySettings ent $ mkTHEntityDefWith style def
          NewtypeD _ _ _ _ _ -> error "Newtypes are not supported"
          _ -> error $ "Unknown declaration type: " ++ show name ++ " " ++ show x
      _        -> error $ "Only datatypes can be processed: " ++ show name
  return $ concat entitiesDecs

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
      , thConstrParams = maybe thConstrParams (f thConstrParams) $ psConstrParams settings
      , thConstrConstrs = maybe thConstrConstrs id $ psConstrConstrs settings
      } where
  f = foldr $ replaceOne psFieldName fieldName applyFieldSettings
  
applyFieldSettings :: PSFieldDef -> THFieldDef -> THFieldDef
applyFieldSettings settings def@(THFieldDef{..}) =
  def { dbFieldName = maybe dbFieldName id $ psDbFieldName settings
      , exprName = maybe exprName id $ psExprName settings
      , embeddedDef = psEmbeddedDef settings
      }

notUniqueBy :: Eq b => (a -> b) -> [a] -> [b]
notUniqueBy f xs = let xs' = map f xs in nub $ xs' \\ nub xs'

validate :: THEntityDef -> Either String THEntityDef
validate def = do
  let assertUnique f xs what = case notUniqueBy f xs of
        [] -> return ()
        ys -> fail $ "All " ++ what ++ " must be unique: " ++ show ys
  -- we need to validate datatype names because TH just creates unusable fields with spaces
  let isSpaceFree = not . any isSpace
  let assertSpaceFree s what = if isSpaceFree s then return () else fail $ "Spaces in " ++ what ++ " are not allowed: " ++ show s
  let constrs = thConstructors def
  assertUnique thPhantomConstrName constrs "constructor phantom name"
  assertUnique dbConstrName constrs "constructor db name"
  forM_ constrs $ \cdef -> do
    let fields = thConstrParams cdef
    assertSpaceFree (thPhantomConstrName cdef) "constructor phantom name"
    assertUnique exprName fields "expr field name in a constructor"
    assertUnique dbFieldName fields "db field name in a constructor"
    forM_ fields $ \fdef -> assertSpaceFree (exprName fdef) "field expr name"
    case filter (\(PSConstraintDef _ cfields) -> null cfields) $ thConstrConstrs cdef of
      [] -> return ()
      ys -> fail $ "Constraints cannot have no fields: " ++ show ys
  return def

mkTHEntityDef :: Dec -> THEntityDef
mkTHEntityDef = mkTHEntityDefWith persistentNamingStyle

mkTHEntityDefWith :: NamingStyle -> Dec -> THEntityDef
mkTHEntityDefWith (NamingStyle{..}) (DataD _ dname typeVars cons _) =
  THEntityDef dname (mkDbEntityName dname') typeVars constrs where
  constrs = zipWith mkConstr [0..] cons
  dname' = nameBase dname

  mkConstr cNum c = case c of
    (NormalC name params) -> mkConstr' name $ zipWith (mkField (nameBase name)) params [0..]
    (RecC name params) -> mkConstr' name $ zipWith (mkVarField (nameBase name)) params [0..]
    (InfixC _ _ _) -> error "Types with infix constructors are not supported"
    (ForallC _ _ _) -> error "Types with existential quantification are not supported"
   where
    mkConstr' name params = THConstructorDef name (mkPhantomName dname' (nameBase name) cNum) (mkDbConstrName dname' (nameBase name) cNum) params []

    mkField :: String -> StrictType -> Int -> THFieldDef
    mkField cname (_, t) fNum = THFieldDef (apply mkNormalFieldName) (apply mkNormalDbFieldName) (apply mkNormalExprFieldName) t Nothing where
      apply f = f dname' cname cNum fNum
    mkVarField :: String -> VarStrictType -> Int -> THFieldDef
    mkVarField cname (fname, _, t) fNum = THFieldDef fname' (apply mkDbFieldName) (apply mkExprFieldName) t Nothing where
      apply f = f dname' cname cNum fname' fNum
      fname' = nameBase fname
mkTHEntityDefWith _ _ = error "Only datatypes can be processed"

firstLetter :: (Char -> Char) -> String -> String
firstLetter f s = f (head s):tail s

mkDecs :: THEntityDef -> Q [Dec]
mkDecs def = do
  --runIO (print def)
  decs <- fmap concat $ sequence
    [ mkPhantomConstructors def
    , mkPhantomConstructorInstances def
    , mkPersistFieldInstance def
    , mkSinglePersistFieldInstance def
    , mkPersistEntityInstance def
    , mkNeverNullInstance def
    ]
--  runIO $ putStrLn $ pprint decs
  return decs
-- $(reify ''SomeData >>= stringE.show)

mkPhantomConstructors :: THEntityDef -> Q [Dec]
mkPhantomConstructors def = mapM f $ thConstructors def where
  f c = dataD (cxt []) (mkName $ thPhantomConstrName c) [] [] []
  
mkPhantomConstructorInstances :: THEntityDef -> Q [Dec]
mkPhantomConstructorInstances def = sequence $ zipWith f [0..] $ thConstructors def where
  f :: Int -> THConstructorDef -> Q Dec
  f cNum c = instanceD (cxt []) (appT (conT ''Constructor) (conT $ mkName $ thPhantomConstrName c)) [phantomConstrName', phantomConstrNum'] where
    phantomConstrName' = funD 'phantomConstrName [clause [wildP] (normalB $ stringE $ dbConstrName c) []]
    phantomConstrNum' = funD 'phantomConstrNum [clause [wildP] (normalB $ [|cNum  |]) []]

mkPersistEntityInstance :: THEntityDef -> Q [Dec]
mkPersistEntityInstance def = do
  let entity = foldl AppT (ConT (dataName def)) $ map extractType $ thTypeParams def
  
  fields' <- do
    cParam <- newName "c"
    fParam <- newName "f"
    let mkField name field = ForallC [] ([EqualP (VarT cParam) (ConT name), EqualP (VarT fParam) (fieldType field)]) $ NormalC (mkName $ exprName field) []
    let f cdef = map (mkField $ mkName $ thPhantomConstrName cdef) $ thConstrParams cdef
    let constrs = concatMap f $ thConstructors def
    return $ DataInstD [] ''Field [entity, VarT cParam, VarT fParam] constrs []
    
  entityDef' <- do
    v <- newName "v"
    let mkLambda t = [|undefined :: $(forallT (thTypeParams def) (cxt []) [t| $(return entity) -> $(return t) |]) |]
    let types = map extractType $ thTypeParams def
    let typeParams' = listE $ map (\t -> [| dbType ($(mkLambda t) $(varE v)) |]) types
    let mkField c fNum f = do
        a <- newName "a"
        let fname = dbFieldName f
        let nvar = if hasFreeVars (fieldType f)
             then let pat = conP (thConstrName c) $ replicate fNum wildP ++ [varP a] ++ replicate (length (thConstrParams c) - fNum - 1) wildP
                      wildClause = if length (thConstructors def) > 1 then [match wildP (normalB [| undefined |]) []] else []
                  in caseE (varE v) $ [match pat (normalB $ varE a) []] ++ wildClause
             else [| undefined :: $(return $ fieldType f) |]
        [| (fname, $(maybe id (\emb t -> [| applyEmbeddedSettings $(lift emb) $t |]) (embeddedDef f) $ [|dbType $nvar|]) ) |]
    let constrs = listE $ zipWith mkConstructorDef [0..] $ thConstructors def
        mkConstructorDef cNum c@(THConstructorDef _ _ name params conss) = [| ConstructorDef cNum name $(listE $ zipWith (mkField c) [0..] params) $(listE $ map (mkConstraint params) conss) |]
        mkConstraint params (PSConstraintDef name fields) = [| Constraint name $(listE $ map (lift . getFieldName params) fields) |]
        getFieldName params name = case filter ((== name) . fieldName) params of
          [f] -> dbFieldName f
          []  -> error $ "Database field name " ++ show name ++ " declared in constraint not found"
          _   -> error $ "It can never happen. Found several fields with one database name " ++ show name
    
    let paramNames = foldr1 (\p xs -> [| $p ++ "$" ++ $xs |] ) $ map (\t -> [| persistName ($(mkLambda t) $(varE v)) |]) types
    let fullEntityName = case null types of
         True  -> [| $(stringE $ dbEntityName def) |]
         False -> [| $(stringE $ dbEntityName def) ++ "$" ++ $(paramNames) |]

    let body = normalB [| EntityDef $fullEntityName $typeParams' $constrs |]
    let pat = if null $ thTypeParams def then wildP else varP v
    funD 'entityDef $ [ clause [pat] body [] ]

  toEntityPersistValues' <- liftM (FunD 'toEntityPersistValues) $ forM (zip [0..] $ thConstructors def) $ \(cNum, c) -> do
    vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, fieldType f)) $ thConstrParams c
    let pat = conP (thConstrName c) $ map (varP . fst) vars
    (lastPrims, fields) <- spanM (isPrim . snd) $ reverse vars
    let lastPrims' = map (appE (varE 'toPrim) . varE . fst) $ reverse $ lastPrims
    let body = if null fields
        then [| return $ $(listE $ [|toPrim (cNum :: Int)|]:lastPrims') |]
        else do
          let go (m, f) (fname, t) = isPrim t >>= \isP -> if isP
              then return (m, [| (toPrim $(varE fname):) |]:f)
              else newName "x" >>= \x -> return (bindS (varP x) [| toPersistValues $(varE fname) |]:m, varE x:f)
          (stmts, func) <- foldM go ([], []) fields        -- foldM puts reversed fields in normal order
          doE $ stmts ++ [noBindS [| return $ (toPrim (cNum :: Int):) . $(foldr1 (\a b -> [|$a . $b|]) func) $ $(listE lastPrims') |]]
    clause [pat] (normalB body) []

  fromEntityPersistValues' <- let
    goField xs vars result failure = do
      (fields, rest) <- spanM (liftM not . isPrim . snd) vars
      xss <- liftM (xs:) $ mapM (const $ newName "xs") fields
      let f oldXs newXs (fname, _) = bindS (conP '(,) [varP fname, varP newXs]) [| fromPersistValues $(varE oldXs) |]
      let stmts = zipWith3 f xss (tail xss) fields
      doE $ stmts ++ [noBindS $ goPrim (last xss) rest result failure]
    goPrim xs vars result failure = do
      xs' <- newName "xs"
      (prim, rest) <- spanM (isPrim . snd) vars
      let (pat, body') = if null rest then (conP '[] [], result) else (varP xs', goField xs' rest result failure)
      let m = match (foldr (\(fname, _) p -> infixP (varP fname) '(:) p) pat prim) (normalB body') []
      caseE (varE xs) [m, failure]
    mkArg (fname, t) = isPrim t >>= \isP -> (if isP then appE (varE 'fromPrim) else id) (varE fname)
    in do
      xs <- newName "xs"
      let failureBody = normalB [| (\a -> fail (failMessage a $(varE xs)) >> return a) undefined |]
      failureName <- newName "failure"
      let failure = match wildP (normalB $ varE failureName) []
      matches <- forM (zip [0..] (thConstructors def)) $ \(cNum, c) -> do
        vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, fieldType f)) $ thConstrParams c
        let result = appE (varE 'return) $ foldl (\a f -> appE a $ mkArg f) (conE $ thConstrName c) vars
        let cNum' = conP 'PersistInt64 [litP $ integerL cNum]
        xs' <- newName "xs"
        (prim, rest) <- spanM (isPrim . snd) vars
        let (pat, body') = if null rest then (conP '[] [], result) else (varP xs', goField xs' rest result failure)
        return $ match (infixP cNum' '(:) $ foldr (\(fname, _) p -> infixP (varP fname) '(:) p) pat prim) (normalB body') []
      let start = caseE (varE xs) $ matches ++ [failure]
      let failureFunc = funD failureName [clause [] failureBody []]
      funD 'fromEntityPersistValues [clause [varP xs] (normalB start) [failureFunc]]

  --TODO: support constraints with embedded datatypes fields
  getConstraints' <- let
    hasConstraints = not . null . thConstrConstrs
    clauses = zipWith mkClause [0::Int ..] (thConstructors def)
    mkClause cNum cdef | not (hasConstraints cdef) = clause [conP (thConstrName cdef) pats] (normalB [| (cNum, []) |]) [] where
      pats = map (const wildP) $ thConstrParams cdef
    mkClause cNum cdef = do
      let allConstrainedFields = concatMap psConstraintFields $ thConstrConstrs cdef
      names <- mapM (\name -> newName name >>= \name' -> return (name, name `elem` allConstrainedFields, name')) $ map fieldName $ thConstrParams cdef
      let body = normalB $ [| (cNum, $(listE $ map (\(PSConstraintDef cname fnames) -> [|(cname, $(listE $ map (\fname -> [| toPrim $(varE $ getFieldName fname) |] ) fnames )) |] ) $ thConstrConstrs cdef)) |]
          getFieldName name = case filter (\(a, _, _) -> a == name) names of
            [(_, _, name')] -> name'
            []  -> error $ "Database field name " ++ show name ++ " declared in constraint not found"
            _   -> error $ "It can never happen. Found several fields with one database name " ++ show name
          pattern = map (\(_, isConstrained, name') -> if isConstrained then varP name' else wildP) names
      clause [conP (thConstrName cdef) pattern] body []
    in funD 'getConstraints clauses
     
  entityFieldChain' <- let
    fieldNames = thConstructors def >>= thConstrParams
    clauses = map (\f -> mkChain f >>= \(fArg, body) -> clause [maybe id asP fArg $ conP (mkName $ exprName f) []] (normalB body) []) fieldNames
    mkChain f = isPrim (fieldType f) >>= \isP -> if isP
      then return (Nothing, [| Left $(lift $ dbFieldName f) |])
      else do
        fArg <- newName "f"
        let body = [| Right [ ($(lift $ dbFieldName f), dbType $ (undefined :: Field v c a -> a) $ $(varE fArg)) ] |]
        return (Just fArg, body)
    in funD 'entityFieldChain clauses

  let context = paramsContext def
  let decs = [fields', entityDef', toEntityPersistValues', fromEntityPersistValues', getConstraints', entityFieldChain']
  return $ [InstanceD context (AppT (ConT ''PersistEntity) entity) decs]

mkPersistFieldInstance :: THEntityDef -> Q [Dec]
mkPersistFieldInstance def = do
  let types = map extractType $ thTypeParams def
  let entity = foldl AppT (ConT (dataName def)) types
  
  persistName' <- do
    v <- newName "v"
    let mkLambda t = [|undefined :: $(forallT (thTypeParams def) (cxt []) [t| $(return entity) -> $(return t) |]) |]
    
    let paramNames = foldr1 (\p xs -> [| $p ++ "$" ++ $xs |] ) $ map (\t -> [| persistName ($(mkLambda t) $(varE v)) |]) types
    let fullEntityName = case null types of
         True  -> [| $(stringE $ dbEntityName def) |]
         False -> [| $(stringE $ dbEntityName def) ++ "$" ++ $(paramNames) |]
    let body = normalB $ fullEntityName
    let pat = if null $ thTypeParams def then wildP else varP v
    funD 'persistName $ [ clause [pat] body [] ]
  
  toPersistValues' <- do
    let body = normalB [| liftM (:) . toSinglePersistValue |]
    funD 'toPersistValues $ [ clause [] body [] ]
  fromPersistValue' <- do
    x <- newName "x"
    xs <- newName "xs"
    let body = normalB [| fromSinglePersistValue $(varE x) >>= \a -> return (a, $(varE xs)) |]
    funD 'fromPersistValues $ [ clause [infixP (varP x) '(:) (varP xs)] body [], clause [wildP] (normalB [| error "fromPersistValue: empty list" |]) [] ]
  dbType' <- funD 'dbType $ [ clause [] (normalB [| DbEntity . entityDef |]) [] ]

  let context = paramsContext def
  let decs = [persistName', toPersistValues', fromPersistValue', dbType']
  return $ [InstanceD context (AppT (ConT ''PersistField) entity) decs]

mkSinglePersistFieldInstance :: THEntityDef -> Q [Dec]
mkSinglePersistFieldInstance def = do
  let types = map extractType $ thTypeParams def
  let entity = foldl AppT (ConT (dataName def)) types

  toSinglePersistValue' <- do
    let body = normalB [| liftM (either toPrim toPrim) . insertBy |]
    funD 'toSinglePersistValue $ [ clause [] body [] ]
  fromSinglePersistValue' <- do
    x <- newName "x"
    let body = normalB [| get (fromPrim $(varE x)) >>= maybe (fail $ "No data with id " ++ show $(varE x)) return |]
    funD 'fromSinglePersistValue $ [ clause [varP x] body []]

  let context = paramsContext def
  let decs = [toSinglePersistValue', fromSinglePersistValue']
  return $ [InstanceD context (AppT (ConT ''SinglePersistField) entity) decs]

mkNeverNullInstance :: THEntityDef -> Q [Dec]
mkNeverNullInstance def = do
  let types = map extractType $ thTypeParams def
  let entity = foldl AppT (ConT (dataName def)) types
  let context = paramsContext def
  return $ [InstanceD context (AppT (ConT ''NeverNull) entity) []]

paramsContext :: THEntityDef -> Cxt
paramsContext def = classPred ''PersistField params ++ classPred ''SinglePersistField maybys ++ classPred ''NeverNull maybys where
  classPred clazz = map (\t -> ClassP clazz [t])
  -- every type must be an instance of PersistField
  params = map extractType $ thTypeParams def
  -- all datatype fields also must be instances of PersistField
  -- if Maybe is applied to a type param, the param must be also an instance of NeverNull
  -- so that (Maybe param) is an instance of PersistField
  maybys = nub $ thConstructors def >>= thConstrParams >>= insideMaybe . fieldType

extractType :: TyVarBndr -> Type
extractType (PlainTV name) = VarT name
extractType (KindedTV name _) = VarT name

#if MIN_VERSION_template_haskell(2, 7, 0)
#define isClassInstance isInstance
#endif

isPrim :: Type -> Q Bool
-- we cannot use simply isClassInstance because it crashes on type vars and in this case
-- class PrimitivePersistField a
-- instance PrimitivePersistField Int
-- instance PrimitivePersistField a => Maybe a
-- it will consider (Maybe anytype) instance of PrimitivePersistField
isPrim t | hasFreeVars t = return False
isPrim t@(ConT _) = isClassInstance ''PrimitivePersistField [t]
isPrim (AppT (ConT key) _)  | key == ''Key = return True
isPrim (AppT (ConT tcon) t) | tcon == ''Maybe = isPrim t
isPrim _ = return False

foldType :: (Type -> a) -> (a -> a -> a) -> Type -> a
foldType f (<>) = go where
  go (ForallT _ _ _) = error "forall'ed fields are not allowed"
  go z@(AppT a b)    = f z <> go a <> go b
  go z@(SigT t _)    = f z <> go t
  go z               = f z

hasFreeVars :: Type -> Bool
hasFreeVars = foldType f (||) where
  f (VarT _) = True
  f _ = False

insideMaybe :: Type -> [Type]
insideMaybe = foldType f (++) where
  f (AppT (ConT c) t@(VarT _)) | c == ''Maybe = [t]
  f _ = []

spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM p = go  where
  go [] = return ([], [])
  go (x:xs) = do
    flg <- p x
    if flg then do
        (ys, zs) <- go xs
        return (x:ys, zs)
      else return ([], x:xs)

-- | Converts quasiquoted settings into the datatype used by mkPersist. The settings are represented in YAML.
-- Unless the property is marked as mandatory, it can be omitted. In this case value created by the NamingStyle will be used.
-- The settings below have all properties set explicitly.
--
-- @
--data Settable = First {foo :: String, bar :: Int} deriving (Eq, Show)
--
--mkPersist fieldNamingStyle [groundhog|
--definitions:                           # Optional header before the definitions list
--                                       # The list elements start with -
--  - entity: Settable                   # Mandatory. Entity datatype name
--    dbName: Settable                   # Name of the main table
--    constructors:                      # List of constructors. The constructors you don't change can be omitted
--      - name: First                    # Mandatory. Constructor name
--        phantomName: FooBarConstructor # Constructor phantom type name used to guarantee type safety
--        dbName: First                  # Name of constructor table which is created only for datatypes with multiple constructors
--        constrParams:                  # List of constructor parameters. The parameters you don't change can be omitted
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
--mkPersist fieldNamingStyle [groundhog|
--entity: Settable                       # If we did not want to add a constraint, this line would be enough
--constructors:
--  - name: First
--    constraints:
--      - name: someconstraint
--        fields: [foo, bar]
-- |]
-- @
groundhog :: QuasiQuoter
groundhog = QuasiQuoter { quoteExp  = parseSettings
                        , quotePat  = error "groundhog: pattern quasiquoter"
                        , quoteType = error "groundhog: type quasiquoter"
                        , quoteDec  = error "groundhog: declaration quasiquoter"
                        }

-- | Parses configuration stored in the file
-- > mkPersist fieldNamingStyle [groundhogFile|../groundhog.yaml|]
groundhogFile :: QuasiQuoter
groundhogFile = quoteFile groundhog

parseSettings :: String -> Q Exp
parseSettings s = either fail lift result where
  result = decodeEither $ pack s :: Either String PersistSettings
