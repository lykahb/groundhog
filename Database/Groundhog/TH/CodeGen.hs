{-# LANGUAGE TemplateHaskell, RecordWildCards, DoAndIfThenElse #-}
{-# LANGUAGE CPP #-}

module Database.Groundhog.TH.CodeGen
  ( mkEmbeddedPersistFieldInstance
  , mkEmbeddedPurePersistFieldInstance
  , mkEmbeddedExpressionInstance
  , mkEmbeddedInstance
  , mkEntityPhantomConstructors
  , mkEntityPhantomConstructorInstances
  , mkEntityPersistFieldInstance
  , mkEntitySinglePersistFieldInstance
  , mkPersistEntityInstance
  , mkEntityNeverNullInstance
  ) where
  
import Database.Groundhog.Core (PersistEntity(..), Embedded(..), Key, PersistField(..), SinglePersistField(..), PurePersistField(..), PrimitivePersistField(..), PersistBackend(..), DbType(..), Constraint(..), Constructor(..), EntityDef(..), ConstructorDef(..), PersistValue(..), NeverNull, Expression(..), Expr(..))
import Database.Groundhog.Generic (failMessage, applyEmbeddedDbTypeSettings)
import Database.Groundhog.TH.Settings
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Control.Monad (liftM, forM, foldM, filterM)
import Data.List (nub)

mkEmbeddedPersistFieldInstance :: THEmbeddedDef -> Q [Dec]
mkEmbeddedPersistFieldInstance def = do
  let types = map extractType $ thEmbeddedTypeParams def
  let embedded = foldl AppT (ConT (embeddedName def)) types
  
  persistName' <- do
    v <- newName "v"
    let mkLambda t = [|undefined :: $(forallT (thEmbeddedTypeParams def) (cxt []) [t| $(return embedded) -> $(return t) |]) |]
    let paramNames = foldr1 (\p xs -> [| $p ++ "$" ++ $xs |] ) $ map (\t -> [| persistName ($(mkLambda t) $(varE v)) |]) types
    let fullEmbeddedName = case null types of
         True  -> [| $(stringE $ dbEmbeddedName def) |]
         False -> [| $(stringE $ dbEmbeddedName def) ++ "$" ++ $(paramNames) |]
    let body = normalB $ fullEmbeddedName
    let pat = if null types then wildP else varP v
    funD 'persistName $ [ clause [pat] body [] ]
    
  -- TODO: remove ([]++) from
  -- data D a = D a; do { x_a3vQ <- toPersistValues x_a3vP;(return $ (x_a3vQ . ([] ++))) }
  toPersistValues' <- do
    vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, fieldType f)) $ embeddedFields def
    let pat = conP (embeddedConstructorName def) $ map (varP . fst) vars
    (lastPrims, fields) <- spanM (isPrim . snd) $ reverse vars
    let lastPrims' = map (appE (varE 'toPrim) . varE . fst) $ reverse $ lastPrims
    let body = if null fields
        then [| return $ ($(listE lastPrims')++) |]
        else do
          let go (m, f) (fname, t) = isPrim t >>= \isP -> if isP
              then return (m, [| (toPrim $(varE fname):) |]:f)
              else newName "x" >>= \x -> return (bindS (varP x) [| toPersistValues $(varE fname) |]:m, varE x:f)
          (stmts, func) <- foldM go ([], []) fields        -- foldM puts reversed fields in normal order
          doE $ stmts ++ [noBindS [| return $ $(foldr1 (\a b -> [|$a . $b|]) func) . ($(listE lastPrims')++) |]]
    funD 'toPersistValues [clause [pat] (normalB body) []]
  
  fromPersistValues' <- let
    goField xs vars result failure = do
      (fields, rest) <- spanM (liftM not . isPrim . snd) vars
      xss <- liftM (xs:) $ mapM (const $ newName "xs") fields
      let f oldXs newXs (fname, _) = bindS (conP '(,) [varP fname, varP newXs]) [| fromPersistValues $(varE oldXs) |]
      let stmts = zipWith3 f xss (tail xss) fields
      (isFailureUsed, expr) <- goPrim (last xss) rest result failure
      return (isFailureUsed, doE $ stmts ++ [noBindS expr])
    goPrim xs vars result failure = do
      xs' <- newName "xs"
      (prim, rest) <- spanM (isPrim . snd) vars
      (isFailureUsed, body') <- case rest of
        [] -> return (False, [| return ($result, $(varE xs')) |])
        _  -> goField xs' rest result failure
      let m = match (foldr (\(fname, _) p -> infixP (varP fname) '(:) p) (varP xs') prim) (normalB body') []
      return $ if not (null rest || null prim)
         then (True, caseE (varE xs) [m, failure])
         else (isFailureUsed, caseE (varE xs) [m])
    mkArg (fname, t) = isPrim t >>= \isP -> (if isP then appE (varE 'fromPrim) else id) (varE fname)
    in do
      xs <- newName "xs"
      let failureBody = normalB [| (\a -> fail (failMessage a $(varE xs)) >> return (a, [])) undefined |]
      failureName <- newName "failure"
      vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, fieldType f)) $ embeddedFields def
      let failure = match wildP (normalB $ varE failureName) []
      let result = foldl (\a f -> appE a $ mkArg f) (conE $ embeddedConstructorName def) vars
      (isFailureUsed, start) <- goPrim xs vars result failure
      let failureFunc = funD failureName [clause [] failureBody []]
      let locals = if isFailureUsed then [failureFunc] else []
      funD 'fromPersistValues [clause [varP xs] (normalB start) locals]
  
  dbType' <- do
    v <- newName "v"
    let mkField fNum f = do
        a <- newName "a"
        let fname = dbFieldName f
        let nvar = if hasFreeVars (fieldType f)
             then let pat = conP (embeddedConstructorName def) $ replicate fNum wildP ++ [varP a] ++ replicate (length (embeddedFields def) - fNum - 1) wildP
                  in caseE (varE v) $ [match pat (normalB $ varE a) []]
             else [| undefined :: $(return $ fieldType f) |]
        case embeddedDef f of
          Nothing -> [| (fname, dbType $nvar) |]
          Just e  -> [| (fname, applyEmbeddedDbTypeSettings $(lift e) (dbType $nvar )) |]
    let pat = if null $ thEmbeddedTypeParams def then wildP else varP v
    funD 'dbType $ [ clause [pat] (normalB [| DbEmbedded False $(listE $ zipWith mkField [0..] (embeddedFields def)) |]) [] ]

  let context = paramsContext (thEmbeddedTypeParams def) (embeddedFields def)
  let decs = [persistName', toPersistValues', fromPersistValues', dbType']
  return $ [InstanceD context (AppT (ConT ''PersistField) embedded) decs]

mkEmbeddedPurePersistFieldInstance :: THEmbeddedDef -> Q [Dec]
mkEmbeddedPurePersistFieldInstance def = do
  let types = map extractType $ thEmbeddedTypeParams def
  let embedded = foldl AppT (ConT (embeddedName def)) types
  
  toPurePersistValues' <- do
    vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, fieldType f)) $ embeddedFields def
    let pat = conP (embeddedConstructorName def) $ map (varP . fst) vars
    let result = map (\(v, t) -> isPrim t >>= \isP -> if isP then [| (toPrim $(varE v):) |] else [| toPurePersistValues $(varE v) |]) vars
    let body = foldr1 (\a b -> [|$a . $b|]) result
    funD 'toPurePersistValues [clause [pat] (normalB body) []]

  fromPurePersistValues' <- let
    goField xs vars result failure = do
      (fields, rest) <- spanM (liftM not . isPrim . snd) vars
      xss <- liftM (xs:) $ mapM (const $ newName "xs") fields
      let f oldXs newXs (fname, _) = valD (conP '(,) [varP fname, varP newXs]) (normalB [| fromPurePersistValues $(varE oldXs) |]) []
      let stmts = zipWith3 f xss (tail xss) fields
      (isFailureUsed, expr) <- goPrim (last xss) rest result failure
      return (isFailureUsed, letE stmts expr)
    goPrim xs vars result failure = do
      xs' <- newName "xs"
      (prim, rest) <- spanM (isPrim . snd) vars
      (isFailureUsed, body') <- case rest of
        [] -> return (False, [| ($result, $(varE xs')) |])
        _  -> goField xs' rest result failure
      let m = match (foldr (\(fname, _) p -> infixP (varP fname) '(:) p) (varP xs') prim) (normalB body') []
      return $ if not (null rest || null prim)
         then (True, caseE (varE xs) [m, failure])
         else (isFailureUsed, caseE (varE xs) [m])
    mkArg (fname, t) = isPrim t >>= \isP -> (if isP then appE (varE 'fromPrim) else id) (varE fname)
    in do
      xs <- newName "xs"
      let failureBody = normalB [| (\a -> error (failMessage a $(varE xs)) `asTypeOf` (a, [])) undefined |]
      failureName <- newName "failure"
      vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, fieldType f)) $ embeddedFields def
      let failure = match wildP (normalB $ varE failureName) []
      let result = foldl (\a f -> appE a $ mkArg f) (conE $ embeddedConstructorName def) vars
      (isFailureUsed, start) <- goPrim xs vars result failure
      let failureFunc = funD failureName [clause [] failureBody []]
      let locals = if isFailureUsed then [failureFunc] else []
      funD 'fromPurePersistValues [clause [varP xs] (normalB start) locals]
  
  context <- paramsPureContext (thEmbeddedTypeParams def) (embeddedFields def)
  case context of
    Nothing -> return []
    Just context' -> do
      let decs = [toPurePersistValues', fromPurePersistValues']
      return $ [InstanceD context' (AppT (ConT ''PurePersistField) embedded) decs]

mkEmbeddedExpressionInstance :: THEmbeddedDef -> Q [Dec]
mkEmbeddedExpressionInstance def = do
  let types = map extractType $ thEmbeddedTypeParams def
  let embedded = foldl AppT (ConT (embeddedName def)) types
  -- funcE is left by default
  let funcA' = TySynInstD ''FuncA [embedded] embedded
  wrap' <- funD 'wrap [clause [] (normalB [|ExprPure|]) []]

  context <- paramsPureContext (thEmbeddedTypeParams def) (embeddedFields def)
  case context of
    Nothing -> return []
    Just context' -> do
      let decs = [funcA', wrap']
      return $ [InstanceD context' (AppT (ConT ''Expression) embedded) decs]  

mkEmbeddedInstance :: THEmbeddedDef -> Q [Dec]
mkEmbeddedInstance def = do
  let types = map extractType $ thEmbeddedTypeParams def
  let embedded = foldl AppT (ConT (embeddedName def)) types
  
  fields' <- do
    fParam <- newName "f"
    let mkField field = ForallC [] ([EqualP (VarT fParam) (fieldType field)]) $ NormalC (mkName $ exprName field) []
    return $ DataInstD [] ''Selector [embedded, VarT fParam] (map mkField $ embeddedFields def) []

  selectorNum' <- do
    let mkClause fNum field = clause [conP (mkName $ exprName field) []] (normalB $ lift fNum) []
    let clauses = zipWith mkClause [0 :: Int ..] (embeddedFields def)
    funD 'selectorNum clauses

  let context = paramsContext (thEmbeddedTypeParams def) (embeddedFields def)
  let decs = [fields', selectorNum']
  return $ [InstanceD context (AppT (ConT ''Embedded) embedded) decs]

mkEntityPhantomConstructors :: THEntityDef -> Q [Dec]
mkEntityPhantomConstructors def = mapM f $ thConstructors def where
  f c = dataD (cxt []) (mkName $ thPhantomConstrName c) [] [] []
  
mkEntityPhantomConstructorInstances :: THEntityDef -> Q [Dec]
mkEntityPhantomConstructorInstances def = sequence $ zipWith f [0..] $ thConstructors def where
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
    let f cdef = map (mkField $ mkName $ thPhantomConstrName cdef) $ thConstrFields cdef
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
             then let pat = conP (thConstrName c) $ replicate fNum wildP ++ [varP a] ++ replicate (length (thConstrFields c) - fNum - 1) wildP
                      wildClause = if length (thConstructors def) > 1 then [match wildP (normalB [| undefined |]) []] else []
                  in caseE (varE v) $ [match pat (normalB $ varE a) []] ++ wildClause
             else [| undefined :: $(return $ fieldType f) |]
        case embeddedDef f of
          Nothing -> [| (fname, dbType $nvar) |]
          Just e  -> [| (fname, applyEmbeddedDbTypeSettings $(lift e) (dbType $nvar )) |]
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
    vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, fieldType f)) $ thConstrFields c
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
        vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, fieldType f)) $ thConstrFields c
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
      pats = map (const wildP) $ thConstrFields cdef
    mkClause cNum cdef = do
      let allConstrainedFields = concatMap psConstraintFields $ thConstrConstrs cdef
      names <- mapM (\name -> newName name >>= \name' -> return (name, name `elem` allConstrainedFields, name')) $ map fieldName $ thConstrFields cdef
      let body = normalB $ [| (cNum, $(listE $ map (\(PSConstraintDef cname fnames) -> [|(cname, $(listE $ map (\fname -> [| toPrim $(varE $ getFieldName fname) |] ) fnames )) |] ) $ thConstrConstrs cdef)) |]
          getFieldName name = case filter (\(a, _, _) -> a == name) names of
            [(_, _, name')] -> name'
            []  -> error $ "Database field name " ++ show name ++ " declared in constraint not found"
            _   -> error $ "It can never happen. Found several fields with one database name " ++ show name
          pattern = map (\(_, isConstrained, name') -> if isConstrained then varP name' else wildP) names
      clause [conP (thConstrName cdef) pattern] body []
    in funD 'getConstraints clauses
     
  entityFieldChain' <- let
    fieldNames = thConstructors def >>= thConstrFields
    clauses = map (\f -> mkChain f >>= \(fArg, body) -> clause [maybe id asP fArg $ conP (mkName $ exprName f) []] (normalB body) []) fieldNames
    mkChain f = isPrim (fieldType f) >>= \isP -> if isP
      then return (Nothing, [| Left $(lift $ dbFieldName f) |])
      else do
        fArg <- newName "f"
        let nvar = [| (undefined :: Field v c a -> a) $(varE fArg) |]
        let typ = case embeddedDef f of
              Nothing -> [| dbType $nvar |]
              Just e  -> [| applyEmbeddedDbTypeSettings $(lift e) (dbType $nvar ) |]
        let body = [| Right [ ($(lift $ dbFieldName f), $typ) ] |]
        return (Just fArg, body)
    in funD 'entityFieldChain clauses

  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  let decs = [fields', entityDef', toEntityPersistValues', fromEntityPersistValues', getConstraints', entityFieldChain']
  return $ [InstanceD context (AppT (ConT ''PersistEntity) entity) decs]

mkEntityPersistFieldInstance :: THEntityDef -> Q [Dec]
mkEntityPersistFieldInstance def = do
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
    let pat = if null types then wildP else varP v
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

  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  let decs = [persistName', toPersistValues', fromPersistValue', dbType']
  return $ [InstanceD context (AppT (ConT ''PersistField) entity) decs]

mkEntitySinglePersistFieldInstance :: THEntityDef -> Q [Dec]
mkEntitySinglePersistFieldInstance def = do
  let types = map extractType $ thTypeParams def
  let entity = foldl AppT (ConT (dataName def)) types

  toSinglePersistValue' <- do
    let body = normalB [| liftM (either toPrim toPrim) . insertBy |]
    funD 'toSinglePersistValue $ [ clause [] body [] ]
  fromSinglePersistValue' <- do
    x <- newName "x"
    let body = normalB [| get (fromPrim $(varE x)) >>= maybe (fail $ "No data with id " ++ show $(varE x)) return |]
    funD 'fromSinglePersistValue $ [ clause [varP x] body []]

  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  let decs = [toSinglePersistValue', fromSinglePersistValue']
  return $ [InstanceD context (AppT (ConT ''SinglePersistField) entity) decs]

mkEntityNeverNullInstance :: THEntityDef -> Q [Dec]
mkEntityNeverNullInstance def = do
  let types = map extractType $ thTypeParams def
  let entity = foldl AppT (ConT (dataName def)) types
  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  return $ [InstanceD context (AppT (ConT ''NeverNull) entity) []]

paramsContext :: [TyVarBndr] -> [THFieldDef] -> Cxt
paramsContext types fields = classPred ''PersistField params ++ classPred ''SinglePersistField maybys ++ classPred ''NeverNull maybys where
  classPred clazz = map (\t -> ClassP clazz [t])
  -- every type must be an instance of PersistField
  params = map extractType types
  -- all datatype fields also must be instances of PersistField
  -- if Maybe is applied to a type param, the param must be also an instance of NeverNull
  -- so that (Maybe param) is an instance of PersistField
  maybys = nub $ fields >>= insideMaybe . fieldType

paramsPureContext :: [TyVarBndr] -> [THFieldDef] -> Q (Maybe Cxt)
paramsPureContext types fields = do
  let isValidType (VarT _) = return True
      isValidType t = isPrim t
  invalid <- filterM (liftM not . isValidType . fieldType) fields
  return $ case invalid of
    [] -> Just $ classPred ''PurePersistField params ++ classPred ''PrimitivePersistField maybys ++ classPred ''NeverNull maybys where
          params = map extractType types
          classPred clazz = map (\t -> ClassP clazz [t])
          -- all datatype fields also must be instances of PersistField
          -- if Maybe is applied to a type param, the param must be also an instance of NeverNull
          -- so that (Maybe param) is an instance of PersistField
          maybys = nub $ fields >>= insideMaybe . fieldType
    _  -> Nothing

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