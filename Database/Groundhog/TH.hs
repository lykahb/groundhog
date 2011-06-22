{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functions to generate the auxiliary structures for the user data type
module Database.Groundhog.TH
  ( deriveEntity
  , setDbEntityName
  , setConstructor
  , setPhantomName
  , setDbConstrName
  , setConstraints
  , setField
  , setDbFieldName
  , setExprFieldName
  ) where

import Database.Groundhog.Core(PersistEntity(..), PersistField(..), NeverNull, Primitive(toPrim), PersistBackend(..), DbType(DbEntity), Constraint, Constructor(..), namedType, EntityDef(..), ConstructorDef(..), PersistValue(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax(StrictType, VarStrictType)
import Control.Monad(liftM, forM, forM_)
import Control.Monad.Trans.State(State, runState, modify)
import Data.Char(toUpper, toLower, isSpace)
import Data.List(nub, (\\))

-- data SomeData a = U1 { foo :: Int} | U2 { bar :: Maybe String, asc :: Int64, add :: a} | U3 deriving (Show, Eq)

data THEntityDef = THEntityDef {
    dataName :: Name -- SomeData
  , dbEntityName :: String  -- SQLSomeData
  , thTypeParams :: [TyVarBndr]
  , thConstructors :: [THConstructorDef]
} deriving Show

data THConstructorDef = THConstructorDef {
    thConstrName    :: Name -- U2
  , thPhantomConstrName :: String -- U2Constructor
  , dbConstrName    :: String -- SQLU2
  , thConstrParams  :: [FieldDef]
  , thConstrConstrs :: [Constraint]
} deriving Show

data FieldDef = FieldDef {
    fieldName :: String -- bar
  , dbFieldName :: String -- SQLbar
  , exprName :: String -- BarField
  , fieldType :: Type
} deriving Show

-- | Set name of the table in the datatype
setDbEntityName :: String -> State THEntityDef ()
setDbEntityName name = modify $ \d -> d {dbEntityName = name}

-- | Modify constructor
setConstructor :: Name -> State THConstructorDef () -> State THEntityDef ()
setConstructor name f = modify $ \d ->
  d {thConstructors = replaceOne thConstrName name f $ thConstructors d}

-- | Set name used to parametrise fields
setPhantomName :: String -> State THConstructorDef ()
setPhantomName name = modify $ \c -> c {thPhantomConstrName = name}

-- | Set name of the constructor specific table
setDbConstrName :: String -> State THConstructorDef ()
setDbConstrName name = modify $ \c -> c {dbConstrName = name}

-- | Set constraints of the constructor. The names should be database names of the fields
setConstraints :: [Constraint] -> State THConstructorDef ()
setConstraints cs = modify $ \c -> c {thConstrConstrs = cs}

-- | Modify field. Field name is a regular field name in record constructor. Otherwise, it is lower-case constructor name with field number.
setField :: String -> State FieldDef () -> State THConstructorDef ()
setField name f = modify $ \c ->
  c {thConstrParams = replaceOne fieldName name f $ thConstrParams c}

-- | Set name of the field column in a database
setDbFieldName :: String -> State FieldDef ()
setDbFieldName name = modify $ \f -> f {dbFieldName = name}

-- | Set name of field constructor used in expressions
setExprFieldName :: String -> State FieldDef ()
setExprFieldName name = modify $ \f -> f {exprName = name}

replaceOne :: (Eq b, Show b) => (a -> b) -> b -> State a () -> [a] -> [a]
replaceOne p a f xs = case length (filter ((==a).p) xs) of
  1 -> map (\x -> if p x == a then runModify f x else x) xs
  0 -> error $ "Element with name " ++ show a ++ " not found"
  _ -> error $ "Found more than one element with name " ++ show a

runModify :: State a () -> a -> a
runModify m a = snd $ runState m a

-- | Creates the auxiliary structures for a user datatype, which are required by Groundhog to manipulate it.
-- 
-- It creates GADT 'Fields' data instance for referring to the fields in
-- expressions and phantom types for data constructors. For record constructors the Field name is the regular field name with 
-- first letter capitalized and postpended \"Field\". If the field is an ordinary constructor, its name is constructor name
-- and postponed field name. The constructor phantom datatypes have the same name as constructors with \"Constructor\" postpended.
--
-- The generation can be adjusted using the optional modifier function. Example:
--
-- > data SomeData a = Normal Int | Record { bar :: Maybe String, asc :: a}
-- > deriveEntity ''SomeData $ Just $ do
-- >   setDbEntityName "SomeTableName"
-- >   setConstructor 'Normal $ do
-- >     setPhantomName "NormalConstructor" -- the same as default
--
-- It will generate these new datatypes and required instances.
--
-- > data NormalConstructor
-- > data RecordConstructor
-- > instance PersistEntity where
-- >   data Fields (SomeData a) where
-- >     Normal0Field :: Fields NormalConstructor Int
-- >     BarField :: Fields RecordConstructor (Maybe String)
-- >     AscField :: Fields RecordConstructor a
-- > ...

deriveEntity :: Name -> Maybe (State THEntityDef ()) -> Q [Dec]
deriveEntity name f = do
  info <- reify name
  let f' = maybe id runModify f
  case info of
    TyConI x -> do
      case x of
        def@(DataD _ _ _ _ _)  -> mkDecs $ either error id $ validate $ f' $ mkTHEntityDef def
        NewtypeD _ _ _ _ _ -> error "Newtypes are not supported"
        _ -> error $ "Unknown declaration type"
    _        -> error "Only datatypes can be processed"

validate :: THEntityDef -> Either String THEntityDef
validate def = do
  let notUniqueBy f xs = let xs' = map f xs in nub $ xs' \\ nub xs'
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
  return def

mkTHEntityDef :: Dec -> THEntityDef
mkTHEntityDef (DataD _ dname typeVars cons _) =
  THEntityDef dname (nameBase dname) typeVars constrs where
  constrs = map mkConstr cons

  mkConstr (NormalC name params) = mkConstr' name $ zipWith (\p i -> mkField (firstLetter toLower (nameBase name) ++ show i) p) params [0::Int ..]
  mkConstr (RecC name params) = mkConstr' name (map mkVarField params)
  mkConstr (InfixC _ _ _) = error "Types with infix constructors are not supported"
  mkConstr (ForallC _ _ _) = error "Types with existential quantification are not supported"
  mkConstr' name params = THConstructorDef name (nameBase name ++ "Constructor") (nameBase name) params []
  
  mkField :: String -> StrictType -> FieldDef
  mkField name (_, t) = mkField' name t
  mkVarField :: VarStrictType -> FieldDef
  mkVarField (name, _, t) = mkField' (nameBase name) t
  mkField' name t = FieldDef name name (firstLetter toUpper name ++ "Field") t
mkTHEntityDef _ = error "Only datatypes can be processed"

firstLetter :: (Char -> Char) -> String -> String
firstLetter f s = f (head s):tail s

mkDecs :: THEntityDef -> Q [Dec]
mkDecs def = do
--  runIO (print def)
  decs <- fmap concat $ sequence
    [ mkPhantomConstructors def
    , mkPhantomConstructorInstances def
    , mkPersistFieldInstance def
    , mkPersistEntityInstance def
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
  let entity = foldl AppT (ConT (dataName def)) $ map getType $ thTypeParams def
  
  fields' <- do
    cParam <- newName "c"
    fParam <- newName "f"
    let mkField name field = ForallC [] ([EqualP (VarT cParam) (ConT name), EqualP (VarT fParam) (fieldType field)]) $ NormalC (mkName $ exprName field) []
    let f cdef = map (mkField $ mkName $ thPhantomConstrName cdef) $ thConstrParams cdef
    let constrs = concatMap f $ thConstructors def
    return $ DataInstD [] ''Fields [entity, VarT cParam, VarT fParam] constrs []
    
  entityDef' <- do
    v <- newName "v"
    let mkLambda t = [|undefined :: $(forallT (thTypeParams def) (cxt []) [t| $(return entity) -> $(return t) |]) |]
    let typeParams' = listE $ map (\t -> [| namedType ($(mkLambda $ getType t) $(varE v)) |]) $ thTypeParams def
    let mkField c fNum f = do
        a <- newName "a"
        let fname = dbFieldName f
        let pats = replicate fNum wildP ++ [varP a] ++ replicate (length (thConstrParams c) - fNum - 1) wildP
        let nvar = case hasFreeVars (fieldType f) of
             True ->  appE (lamE [conP (thConstrName c) pats] (varE a)) (varE v)
             False -> [| undefined :: $(return $ fieldType f) |]
        [| (fname, namedType $nvar) |]
         
    let constrs = listE $ zipWith (\cNum c@(THConstructorDef _ _ name params conss) -> [| ConstructorDef cNum name $(listE $ zipWith (mkField c) [0..] params) conss |]) [0..] $ thConstructors def
    let body = normalB [| EntityDef $(stringE $ dbEntityName def) $typeParams' $constrs |]
    funD 'entityDef $ [ clause [varP v] body [] ]

  toPersistValues' <- liftM (FunD 'toPersistValues) $ forM (zip [0..] $ thConstructors def) $ \(cNum, c) -> do
    names <- mapM (const $ newName "f") $ thConstrParams c
    let pat = conP (thConstrName c) (map varP names)
    let body = normalB $ [| sequence $(listE $ map (appE (varE 'toPersistValue)) ([|cNum::Int|]:map varE names) ) |]
    clause [pat] body []
  
--  fromPersistValues' <- funD 'fromPersistValues $ [ clause [wildP] (normalB [| error "fromPersistValues" |])[] ]
  fromPersistValues' <- do
    clauses <- forM (zip [0..] (thConstructors def)) $ \(cNum, c) -> do
      names <- mapM (const $ newName "x") $ thConstrParams c
      names' <- mapM (const $ newName "x'") $ thConstrParams c
      let pat = conP '(:) [conP 'PersistInt64 [litP $ integerL cNum], listP $ map varP names]
      let result = noBindS (appE (varE 'return) ( foldl (\a -> appE a . varE) (conE (thConstrName c)) names'))
      let getField name name' f = bindS (varP name') [| fromPersistValue $(varE name) |]
      let body = normalB $ doE $ (zipWith3 getField names names' (thConstrParams c)) ++ [result]
--      let body = normalB [|undefined|]
      clause [pat] body []
    unexpected <- newName "xs" >>= \xs -> clause [varP xs] (normalB [| fail $ "Invalid values: " ++ show $(varE xs) |]) []
    return $ FunD 'fromPersistValues $ clauses ++ [unexpected]
  
  getConstraints' <- let
    hasConstraints = not . null . thConstrConstrs
    clauses = zipWith mkClause [0::Int ..] (thConstructors def)
    mkClause cNum cdef | not (hasConstraints cdef) = clause [conP (thConstrName cdef) pats] (normalB [| (cNum, []) |]) [] where
      pats = map (const wildP) $ thConstrParams cdef
    mkClause cNum cdef = clause [conP (thConstrName cdef) (map varP names)] body [] where
      getFieldName n = case filter ((==n).dbFieldName) $ thConstrParams cdef of
        [f] -> varE $ mkName $ fieldName f
        []  -> error $ "Database field name " ++ show n ++ " declared in constraint not found"
        _   -> error $ "It can never happen. Found several fields with one database name " ++ show n
      body = normalB $ [| (cNum, $(listE $ map (\(cname, fnames) -> [|(cname, $(listE $ map (\fname -> [| (fname, toPrim $(getFieldName fname)) |] ) fnames )) |] ) $ thConstrConstrs cdef)) |]
      names = map (mkName . fieldName) $ thConstrParams cdef
    in funD 'getConstraints clauses
  
  showField' <- do
    let fields = concatMap thConstrParams $ thConstructors def
    funD 'showField $ map (\f -> clause [conP (mkName $ exprName f) []] (normalB $ stringE $ dbFieldName f)[] ) fields

  eqField' <- let
    fieldNames = thConstructors def >>= thConstrParams >>= return.mkName.exprName
    clauses = map (\n -> clause [conP n [], conP n []] (normalB [| True |]) []) fieldNames
    in funD 'eqField $ if length clauses > 1
     then clauses ++ [clause [wildP, wildP] (normalB [| False |]) []]
     else clauses

  let context = paramsContext def
  let decs = [fields', entityDef', toPersistValues', fromPersistValues', getConstraints', showField', eqField']
  return $ [InstanceD context (AppT (ConT ''PersistEntity) entity) decs]
  
mkPersistFieldInstance :: THEntityDef -> Q [Dec]
mkPersistFieldInstance def = do
  let types = map getType $ thTypeParams def
  let entity = foldl AppT (ConT (dataName def)) types
  
  persistName' <- do
    v <- newName "v"
    let mkLambda t = [|undefined :: $(forallT (thTypeParams def) (cxt []) [t| $(return entity) -> $(return t) |]) |]
    
    let paramNames = foldr1 (\p xs -> [| $p ++ "$" ++ $xs |] ) $ map (\t -> [| persistName ($(mkLambda t) $(varE v)) |]) types
    let namesList = case null types of
         True  -> [| $(stringE $ dbEntityName def) |]
         False -> [| $(stringE $ dbEntityName def) ++ "$" ++ $(paramNames) |]
    let body = normalB $ namesList
    funD 'persistName $ [ clause [varP v] body [] ]
  
  toPersistValue' <- do
    let body = normalB [| liftM (either toPrim toPrim) . insertBy |]
    funD 'toPersistValue $ [ clause [] body [] ]
  fromPersistValue' <- do
    x <- newName "x"
    let body = normalB [| fromPersistValue $(varE x) >>= get >>= maybe (fail $ "No data with id " ++ show $(varE x)) return |]
    funD 'fromPersistValue $ [ clause [varP x] body [] ]
  dbType' <- funD 'dbType $ [ clause [] (normalB [| DbEntity . entityDef |]) [] ]

  let context = paramsContext def
  let decs = [persistName', toPersistValue', fromPersistValue', dbType']
  return $ [InstanceD context (AppT (ConT ''PersistField) entity) decs]

paramsContext :: THEntityDef -> Cxt
paramsContext def = classPred ''PersistField params ++ classPred ''NeverNull maybys where
  classPred clazz = map (\t -> ClassP clazz [t])
  -- every type must be an instance of PersistField
  params = map getType $ thTypeParams def
  -- all datatype fields also must be instances of PersistField
  -- if Maybe is applied to a type param, the param must be also an instance of NeverNull
  -- so that (Maybe param) is an instance of PersistField
  maybys = nub $ thConstructors def >>= thConstrParams >>= insideMaybe . fieldType

getType :: TyVarBndr -> Type
getType (PlainTV name) = VarT name
getType (KindedTV name _) = VarT name

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
