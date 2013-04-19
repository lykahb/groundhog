{-# LANGUAGE TemplateHaskell, RecordWildCards, DoAndIfThenElse #-}
{-# LANGUAGE CPP #-}

module Database.Groundhog.TH.CodeGen
  ( mkEmbeddedPersistFieldInstance
  , mkEmbeddedPurePersistFieldInstance
  , mkEmbeddedInstance
  , mkEntityPhantomConstructors
  , mkEntityPhantomConstructorInstances
  , mkEntityUniqueKeysPhantoms
  , mkAutoKeyPersistFieldInstance
  , mkAutoKeyPrimitivePersistFieldInstance
  , mkUniqueKeysIsUniqueInstances
  , mkUniqueKeysEmbeddedInstances
  , mkUniqueKeysPersistFieldInstances
  , mkUniqueKeysPrimitiveOrPurePersistFieldInstances
  , mkKeyEqShowInstances
  , mkEntityPersistFieldInstance
  , mkEntitySinglePersistFieldInstance
  , mkPersistEntityInstance
  , mkEntityNeverNullInstance
  , mkMigrateFunction
  ) where
  
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.TH.Settings
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Control.Monad (liftM, liftM2, forM, forM_, foldM, filterM, replicateM)
import Data.List (findIndex, nub, partition)
import Data.Maybe (catMaybes)

mkEmbeddedPersistFieldInstance :: THEmbeddedDef -> Q [Dec]
mkEmbeddedPersistFieldInstance def = do
  let types = map extractType $ thEmbeddedTypeParams def
  let embedded = foldl AppT (ConT (thEmbeddedName def)) types

  persistName' <- do
    v <- newName "v"
    let mkLambda t = [|undefined :: $(forallT (thEmbeddedTypeParams def) (cxt []) [t| $(return embedded) -> $(return t) |]) |]
    let paramNames = foldr1 (\p xs -> [| $p ++ [delim] ++ $xs |] ) $ map (\t -> [| persistName ($(mkLambda t) $(varE v)) |]) types
    let fullEmbeddedName = case null types of
         True  -> [| $(stringE $ thDbEmbeddedName def) |]
         False -> [| $(stringE $ thDbEmbeddedName def) ++ [delim] ++ $(paramNames) |]
    let body = normalB $ fullEmbeddedName
    let pat = if null types then wildP else varP v
    funD 'persistName $ [ clause [pat] body [] ]

  toPersistValues' <- do
    vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, thFieldType f)) $ thEmbeddedFields def
    let pat = conP (thEmbeddedConstructorName def) $ map (varP . fst) vars
    proxy <- newName "p"
    (lastPrims, fields) <- spanM (isPrim . snd) $ reverse vars
    let lastPrims' = map (\(x, _) -> [| toPrimitivePersistValue $(varE proxy) $(varE x) |]) $ reverse $ lastPrims
    let body = if null fields
        then [| return $ ($(listE lastPrims')++) |]
        else do
          let go (m, f) (fname, t) = isPrim t >>= \isP -> if isP
              then return (m, [| (toPrimitivePersistValue $(varE proxy) $(varE fname):) |]:f)
              else newName "x" >>= \x -> return (bindS (varP x) [| toPersistValues $(varE fname) |]:m, varE x:f)
          (stmts, func) <- foldM go ([], []) fields        -- foldM puts reversed fields in normal order
          let nonPrimFields' = foldr1 (\a b -> [|$a . $b|]) func
          let result = if null lastPrims' then nonPrimFields' else [| $nonPrimFields' . ($(listE lastPrims')++) |]
          doE $ stmts ++ [noBindS [| return $ $result |]]
    anyPrim <- liftM or $ mapM (isPrim . snd) vars
    let body' = if anyPrim then [| phantomDb >>= $(lamE [varP proxy] body) |] else body
    funD 'toPersistValues [clause [pat] (normalB body') []]

  fromPersistValues' <- do
    xs <- newName "xs"
    failureName <- newName "failure"
    (isFailureUsed, body) <- mkFromPersistValues failureName xs (thEmbeddedConstructorName def) (thEmbeddedFields def)
    let failureBody = normalB [| (\a -> fail (failMessage a $(varE xs)) >> return (a, [])) undefined |]
        failureFunc = funD failureName [clause [] failureBody []]
        locals = if isFailureUsed then [failureFunc] else []
    funD 'fromPersistValues [clause [varP xs] (normalB $ return body) locals]

  dbType' <- do
    v <- newName "v"
    let mkField fNum f = do
        a <- newName "a"
        let fname = thDbFieldName f
            nvar = if hasFreeVars (thFieldType f)
              then let pat = conP (thEmbeddedConstructorName def) $ replicate fNum wildP ++ [varP a] ++ replicate (length (thEmbeddedFields def) - fNum - 1) wildP
                   in caseE (varE v) $ [match pat (normalB $ varE a) []]
              else [| undefined :: $(return $ thFieldType f) |]
            typ = mkType f nvar
        [| (fname, $typ) |]
    let pat = if null $ thEmbeddedTypeParams def then wildP else varP v
    funD 'dbType $ [ clause [pat] (normalB [| DbEmbedded $ EmbeddedDef False $(listE $ zipWith mkField [0..] (thEmbeddedFields def)) |]) [] ]

  let context = paramsContext (thEmbeddedTypeParams def) (thEmbeddedFields def)
  let decs = [persistName', toPersistValues', fromPersistValues', dbType']
  return $ [InstanceD context (AppT (ConT ''PersistField) embedded) decs]

mkFromPersistValues :: Name -> Name -> Name -> [THFieldDef] -> Q (Bool, Exp)
mkFromPersistValues failureName values constrName fieldDefs = let
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
    mkArg proxy (fname, t) = isPrim t >>= \isP -> (if isP then [| fromPrimitivePersistValue $(varE proxy) $(varE fname) |] else (varE fname))
    in do
      proxy <- newName "p"
      vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, thFieldType f)) fieldDefs
      anyPrim <- liftM or $ mapM (isPrim . snd) vars
      let failure = match wildP (normalB $ varE failureName) []
      let result = foldl (\a f -> appE a $ mkArg proxy f) (conE constrName) vars
      (isFailureUsed, body) <- goPrim values vars result failure
      body' <- if anyPrim then [| phantomDb >>= $(lamE [varP proxy] body) |] else body
      return (isFailureUsed, body')

mkPurePersistFieldInstance :: Type -> Name -> [THFieldDef] -> Cxt -> Q [Dec]
mkPurePersistFieldInstance dataType cName fDefs context = do
  toPurePersistValues' <- do
    vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, thFieldType f)) fDefs
    let pat = conP cName $ map (varP . fst) vars
    proxy <- newName "p"
    let body = mkToPurePersistValues proxy vars
    funD 'toPurePersistValues [clause [varP proxy, pat] (normalB body) []]

  fromPurePersistValues' <- let
    goField xs vars result failure proxy = do
      (fields, rest) <- spanM (liftM not . isPrim . snd) vars
      xss <- liftM (xs:) $ mapM (const $ newName "xs") fields
      let f oldXs newXs (fname, _) = valD (conP '(,) [varP fname, varP newXs]) (normalB [| fromPurePersistValues $(varE proxy) $(varE oldXs) |]) []
      let stmts = zipWith3 f xss (tail xss) fields
      (isFailureUsed, expr) <- goPrim (last xss) rest result failure proxy
      return (isFailureUsed, letE stmts expr)
    goPrim xs vars result failure proxy = do
      xs' <- newName "xs"
      (prim, rest) <- spanM (isPrim . snd) vars
      (isFailureUsed, body') <- case rest of
        [] -> return (False, [| ($result, $(varE xs')) |])
        _  -> goField xs' rest result failure proxy
      let m = match (foldr (\(fname, _) p -> infixP (varP fname) '(:) p) (varP xs') prim) (normalB body') []
      return $ if not (null prim)
         then (True, caseE (varE xs) [m, failure])
         else (isFailureUsed, caseE (varE xs) [m])
    mkArg proxy (fname, t) = isPrim t >>= \isP -> (if isP then [| fromPrimitivePersistValue $(varE proxy) $(varE fname) |] else (varE fname))
    in do
      xs <- newName "xs"
      let failureBody = normalB [| (\a -> error (failMessage a $(varE xs)) `asTypeOf` (a, [])) undefined |]
      failureName <- newName "failure"
      proxy <- newName "p"
      vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, thFieldType f)) fDefs
      let failure = match wildP (normalB $ varE failureName) []
      let result = foldl (\a f -> appE a $ mkArg proxy f) (conE cName) vars
      (isFailureUsed, start) <- goPrim xs vars result failure proxy
      let failureFunc = funD failureName [clause [] failureBody []]
      let locals = if isFailureUsed then [failureFunc] else []
      funD 'fromPurePersistValues [clause [varP proxy, varP xs] (normalB start) locals]

  let decs = [toPurePersistValues', fromPurePersistValues']
  return $ [InstanceD context (AppT (ConT ''PurePersistField) dataType) decs]

mkEmbeddedPurePersistFieldInstance :: THEmbeddedDef -> Q [Dec]
mkEmbeddedPurePersistFieldInstance def = do
  let types = map extractType $ thEmbeddedTypeParams def
  let embedded = foldl AppT (ConT (thEmbeddedName def)) types
  let fDefs = thEmbeddedFields def
  context <- paramsPureContext (thEmbeddedTypeParams def) fDefs
  case context of
    Nothing -> return []
    Just context' -> mkPurePersistFieldInstance embedded (thEmbeddedConstructorName def) fDefs context'

mkAutoKeyPersistFieldInstance :: THEntityDef -> Q [Dec]
mkAutoKeyPersistFieldInstance def = case thAutoKey def of
  Just autoKey -> do
    let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
    keyType <- [t| Key $(return entity) BackendSpecific |]
    
    persistName' <- do
      a <- newName "a"
      let body = [| "Key" ++ [delim] ++ persistName ((undefined :: Key v u -> v) $(varE a)) |]
      funD 'persistName [clause [varP a] (normalB body) []]
    toPersistValues' <- funD 'toPersistValues [clause [] (normalB [| primToPersistValue |]) []]
    fromPersistValues' <- funD 'fromPersistValues [clause [] (normalB [| primFromPersistValue |]) []]
    dbType' <- do
      a <- newName "a"
      let body = [| DbEntity Nothing Nothing Nothing $ entityDef ((undefined :: Key v a -> v) $(varE a)) |]
      funD 'dbType [clause [varP a] (normalB body) []]
    
    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    let decs = [persistName', toPersistValues', fromPersistValues', dbType']
    return [InstanceD context (AppT (ConT ''PersistField) keyType) decs]
  _ -> return []

mkAutoKeyPrimitivePersistFieldInstance :: THEntityDef -> Q [Dec]
mkAutoKeyPrimitivePersistFieldInstance def = case thAutoKey def of
  Just autoKey -> do
    let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
    keyType <- [t| Key $(return entity) BackendSpecific |]
    let conName = mkName $ thAutoKeyConstrName autoKey
    toPrim' <- do
      proxy <- newName "p"
      x <- newName "x"
      let body = [| toPrimitivePersistValue $(varE proxy) $ ((fromPrimitivePersistValue :: DbDescriptor db => Proxy db -> PersistValue -> AutoKeyType db) $(varE proxy)) $(varE x) |]
      funD 'toPrimitivePersistValue [clause [varP proxy, conP conName [varP x]] (normalB body) []]
    fromPrim' <- funD 'fromPrimitivePersistValue [clause [wildP] (normalB $ conE conName) []]
    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    let decs = [toPrim', fromPrim']
    return [InstanceD context (AppT (ConT ''PrimitivePersistField) keyType) decs]
  _ -> return []

mkUniqueKeysIsUniqueInstances :: THEntityDef -> Q [Dec]
mkUniqueKeysIsUniqueInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  let constr = head $ thConstructors def
  forM (thUniqueKeys def) $ \unique -> do
    uniqKeyType <- [t| Key $(return entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique)) |]
    uniqueConstr' <- do
      typ <- conT $ mkName $ thPhantomConstrName constr
      return $ TySynInstD ''UniqueConstr [uniqKeyType] typ
    extractUnique' <- do
      uniqueFields <- mapM (\f -> newName "x" >>= \x -> return (thFieldName f, x)) $ thUniqueKeyFields unique
      let mkFieldPat f = maybe wildP varP $ lookup (thFieldName f) uniqueFields
      let pat = conP (thConstrName constr) $ map mkFieldPat $ thConstrFields constr
      let body = foldl (\expr f -> [| $expr $(varE $ snd f) |]) (conE $ mkName $ thUniqueKeyConstrName unique) uniqueFields
      funD 'extractUnique [clause [pat] (normalB body) []]
    uniqueNum' <- do
      let index = findIndex (\u -> thUniqueKeyName unique == thUniqueName u) $ thConstrUniques constr
      let uNum = maybe (error $ "mkUniqueKeysIsUniqueInstances: cannot find unique definition for unique key " ++ thUniqueKeyName unique) id index
      funD 'uniqueNum [clause [wildP] (normalB [| uNum |]) []]
    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    return $ InstanceD context (AppT (ConT ''IsUniqueKey) uniqKeyType) [uniqueConstr', extractUnique', uniqueNum']

mkUniqueKeysEmbeddedInstances :: THEntityDef -> Q [Dec]
mkUniqueKeysEmbeddedInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  liftM concat $ forM (filter thUniqueKeyMakeEmbedded $ thUniqueKeys def) $ \unique -> do
    uniqKeyType <- [t| Key $(return entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique)) |]
    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    mkEmbeddedInstance' uniqKeyType (thUniqueKeyFields unique) context
  
mkUniqueKeysPersistFieldInstances :: THEntityDef -> Q [Dec]
mkUniqueKeysPersistFieldInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  forM (thUniqueKeys def) $ \unique -> do
    uniqKeyType <- [t| Key $(return entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique)) |]

    persistName' <- funD 'persistName [clause [wildP] (normalB $ stringE $ thUniqueKeyDbName unique) []]

    toPersistValues' <- funD 'toPersistValues [clause [] (normalB [| pureToPersistValue |]) []]

    fromPersistValues' <- funD 'fromPersistValues [clause [] (normalB [| pureFromPersistValue |]) []]

    dbType' <- do
      a <- newName "a"
      let mkField f = do
          let fname = thDbFieldName f
              nvar = [| undefined :: $(return $ thFieldType f) |]
              typ = mkType f nvar
          [| (fname, $typ) |]
      let embedded = [| EmbeddedDef False $(listE $ map mkField $ thUniqueKeyFields unique) |]
      let body = [| DbEntity (Just ($embedded, $(lift $ thUniqueKeyName unique))) Nothing Nothing $ entityDef ((undefined :: Key v a -> v) $(varE a)) |]
      funD 'dbType [clause [varP a] (normalB body) []]
    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    let decs = [persistName', toPersistValues', fromPersistValues', dbType']
    return $ InstanceD context (AppT (ConT ''PersistField) uniqKeyType) decs
    
mkUniqueKeysPrimitiveOrPurePersistFieldInstances :: THEntityDef -> Q [Dec]
mkUniqueKeysPrimitiveOrPurePersistFieldInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  liftM concat $ forM (thUniqueKeys def) $ \unique -> do
    uniqKeyType <- [t| Key $(return entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique)) |]
    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    let conName = mkName $ thUniqueKeyConstrName unique
    isUniquePrim <- if (length $ thUniqueKeyFields unique) == 1
      then isPrim $ thFieldType $ head $ thUniqueKeyFields unique
      else return False
    if isUniquePrim
      then do
        proxy <- newName "p"
        x <- newName "x"
        toPrim' <- do
          funD 'toPrimitivePersistValue [clause [varP proxy, conP conName [varP x]] (normalB [| toPrimitivePersistValue $(varE proxy) $(varE x) |]) []]
        fromPrim' <- funD 'fromPrimitivePersistValue [clause [varP proxy, varP x] (normalB [| $(conE conName) (fromPrimitivePersistValue $(varE proxy) $(varE x)) |]) []]
        let decs = [toPrim', fromPrim']
        return [InstanceD context (AppT (ConT ''PrimitivePersistField) uniqKeyType) decs]
      else mkPurePersistFieldInstance uniqKeyType conName (thUniqueKeyFields unique) context

mkKeyEqShowInstances :: THEntityDef -> Q [Dec]
mkKeyEqShowInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  let mkShowInstance typ cName fieldsNum = do
      showsPrec' <- do
        p <- newName "p"
        fields <- replicateM fieldsNum (newName "x")
        let pat = conP (mkName cName) $ map varP fields
        --let shownArgs = foldr1 (\a b -> [| $a ++ $b |]) $ map (\a -> [| show $(varE a) |]) fields
        --let body = [| $(lift $ cName ++ " ") ++ $shownArgs |]
        
        let showC = [| showString $(lift $ cName ++ " ") |]
        let showArgs = foldr1 (\a b -> [| $a . showString " " . $b |]) $ map (\a -> [| showsPrec 11 $(varE a) |]) fields
        let body = [| showParen ($(varE p) >= (11 :: Int)) ($showC . $showArgs) |]
        funD 'showsPrec [clause [varP p, pat] (normalB body) []]
      let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
      let decs = [showsPrec']
      return $ InstanceD context (AppT (ConT ''Show) typ) decs
  let mkEqInstance typ cName fieldsNum = do
      eq' <- do
        let fields = replicateM fieldsNum (newName "x")
        (fields1, fields2) <- liftM2 (,) fields fields
        let mkPat = conP (mkName cName) . map varP
        let body = foldr1 (\e1 e2 -> [| $e1 && $e2 |]) $ zipWith (\n1 n2 -> [| $(varE n1) == $(varE n2) |]) fields1 fields2
        funD '(==) [clause [mkPat fields1, mkPat fields2] (normalB body) []]
      let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
      let decs = [eq']
      return $ InstanceD context (AppT (ConT ''Eq) typ) decs
    
  autoKeyInstance <- case thAutoKey def of
    Nothing -> return []
    Just autoKey -> do
      keyType <- [t| Key $(return entity) BackendSpecific |]
      mapM (\f -> f keyType (thAutoKeyConstrName autoKey) 1) [mkShowInstance, mkEqInstance]
  uniqsInstances <- forM (thUniqueKeys def) $ \unique -> do
    uniqKeyType <- [t| Key $(return entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique)) |]
    let fieldsNum = length $ thUniqueKeyFields unique
    mapM (\f -> f uniqKeyType (thUniqueKeyConstrName unique) fieldsNum) [mkShowInstance, mkEqInstance]
  return $ autoKeyInstance ++ concat uniqsInstances

mkEmbeddedInstance :: THEmbeddedDef -> Q [Dec]
mkEmbeddedInstance def = do
  let types = map extractType $ thEmbeddedTypeParams def
  let embedded = foldl AppT (ConT (thEmbeddedName def)) types
  let context = paramsContext (thEmbeddedTypeParams def) (thEmbeddedFields def)
  mkEmbeddedInstance' embedded (thEmbeddedFields def) context
  
mkEmbeddedInstance' :: Type -> [THFieldDef] -> Cxt -> Q [Dec]
mkEmbeddedInstance' dataType fDefs context = do
  selector' <- do
    fParam <- newName "f"
    let mkField field = ForallC [] ([EqualP (VarT fParam) (thFieldType field)]) $ NormalC (mkName $ thExprName field) []
    return $ DataInstD [] ''Selector [dataType, VarT fParam] (map mkField fDefs) []

  selectorNum' <- do
    let mkClause fNum field = clause [conP (mkName $ thExprName field) []] (normalB $ lift fNum) []
    let clauses = zipWith mkClause [0 :: Int ..] fDefs
    funD 'selectorNum clauses

  let decs = [selector', selectorNum']
  return $ [InstanceD context (AppT (ConT ''Embedded) dataType) decs]

mkEntityPhantomConstructors :: THEntityDef -> Q [Dec]
mkEntityPhantomConstructors def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  forM (thConstructors def) $ \c -> do
    v <- newName "v"
    let name = mkName $ thPhantomConstrName c
    phantom <- [t| ConstructorMarker $(return entity) |]
    let constr = ForallC (thTypeParams def) [EqualP (VarT v) phantom] $ NormalC name []
    dataD (cxt []) name [PlainTV v] [return constr] []
  
mkEntityPhantomConstructorInstances :: THEntityDef -> Q [Dec]
mkEntityPhantomConstructorInstances def = sequence $ zipWith f [0..] $ thConstructors def where
  f :: Int -> THConstructorDef -> Q Dec
  f cNum c = instanceD (cxt []) (appT (conT ''Constructor) (conT $ mkName $ thPhantomConstrName c)) [phantomConstrName', phantomConstrNum'] where
    phantomConstrName' = funD 'phantomConstrName [clause [wildP] (normalB $ stringE $ thDbConstrName c) []]
    phantomConstrNum' = funD 'phantomConstrNum [clause [wildP] (normalB $ [| cNum |]) []]

mkEntityUniqueKeysPhantoms :: THEntityDef -> Q [Dec]
mkEntityUniqueKeysPhantoms def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  forM (thUniqueKeys def) $ \u -> do
    v <- newName "v"
    let name = mkName $ thUniqueKeyPhantomName u
    phantom <- [t| UniqueMarker $(return entity) |]
    let constr = ForallC (thTypeParams def) [EqualP (VarT v) phantom] $ NormalC name []
    dataD (cxt []) name [PlainTV v] [return constr] []
    
mkPersistEntityInstance :: THEntityDef -> Q [Dec]
mkPersistEntityInstance def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def

  key' <- do
    uParam <- newName "u"
    autoKey <- case thAutoKey def of
      Nothing -> return []
      Just k -> do
        keyDescr <- [t| BackendSpecific |]
        return [ForallC [] [EqualP (VarT uParam) keyDescr] $ NormalC (mkName $ thAutoKeyConstrName k) [(NotStrict, ConT ''PersistValue)]]
    uniques <- forM (thUniqueKeys def) $ \unique -> do
      let cDef = head $ thConstructors def
      uniqType <- [t| Unique $(conT $ mkName $ thUniqueKeyPhantomName unique) |]
      let uniqFieldNames = thUniqueFields $ findOne "unique" thUniqueKeyName thUniqueName unique $ thConstrUniques cDef
      let uniqFields = concat $ flip map uniqFieldNames $ \name -> (filter ((== name) . thFieldName) $ thConstrFields cDef)
      let uniqFields' = map (\f -> (NotStrict, thFieldType f)) uniqFields
      return $ ForallC [] [EqualP (VarT uParam) uniqType] $ NormalC (mkName $ thUniqueKeyConstrName unique) uniqFields'
    return $ DataInstD [] ''Key [entity, VarT uParam] (autoKey ++ uniques) []

  autoKey' <- do
    autoType <- case thAutoKey def of
      Nothing -> conT ''()
      Just k -> [t| Key $(return entity) BackendSpecific |]
    return $ TySynInstD ''AutoKey [entity] autoType
    
  defaultKey' <- do
    let keyType = case thAutoKey def of
         Just k | thAutoKeyIsDef k -> [t| BackendSpecific |]
         _ -> let unique = head $ filter thUniqueKeyIsDef $ thUniqueKeys def
              in  [t| Unique $(conT $ mkName $ thUniqueKeyPhantomName unique) |]
    typ <- [t| Key $(return entity) $keyType |]
    return $ TySynInstD ''DefaultKey [entity] typ
  
  fields' <- do
    cParam <- newName "c"
    fParam <- newName "f"
    let mkField name field = ForallC [] [EqualP (VarT cParam) (ConT name), EqualP (VarT fParam) (thFieldType field)] $ NormalC (mkName $ thExprName field) []
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
        let fname = thDbFieldName f
            nvar = if hasFreeVars (thFieldType f)
              then let pat = conP (thConstrName c) $ replicate fNum wildP ++ [varP a] ++ replicate (length (thConstrFields c) - fNum - 1) wildP
                       wildClause = if length (thConstructors def) > 1 then [match wildP (normalB [| undefined |]) []] else []
                   in caseE (varE v) $ [match pat (normalB $ varE a) []] ++ wildClause
              else [| undefined :: $(return $ thFieldType f) |]
            typ = mkType f nvar
        [| (fname, $typ) |]
    let constrs = listE $ zipWith mkConstructorDef [0..] $ thConstructors def
        mkConstructorDef cNum c@(THConstructorDef _ _ name keyName params conss) = [| ConstructorDef cNum name keyName $(listE $ map snd fields) $(listE $ map mkConstraint conss) |] where
          fields = zipWith (\i f -> (thFieldName f, mkField c i f)) [0..] params
          mkConstraint (THUniqueDef uName uType uFields) = [| UniqueDef uName uType $(listE $ map getField uFields) |]
          getField fName = case lookup fName fields of
            Just f -> f
            Nothing -> error $ "Field name " ++ show fName ++ " declared in unique not found"
    
    let paramNames = foldr1 (\p xs -> [| $p ++ [delim] ++ $xs |] ) $ map (\t -> [| persistName ($(mkLambda t) $(varE v)) |]) types
    let fullEntityName = case null types of
         True  -> [| $(stringE $ thDbEntityName def) |]
         False -> [| $(stringE $ thDbEntityName def) ++ [delim] ++ $(paramNames) |]

    let body = normalB [| EntityDef $fullEntityName $(lift $ thEntitySchema def) $typeParams' $constrs |]
    let pat = if null $ thTypeParams def then wildP else varP v
    funD 'entityDef $ [ clause [pat] body [] ]

  toEntityPersistValues' <- liftM (FunD 'toEntityPersistValues) $ forM (zip [0..] $ thConstructors def) $ \(cNum, c) -> do
    vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, thFieldType f)) $ thConstrFields c
    let pat = conP (thConstrName c) $ map (varP . fst) vars
    proxy <- newName "p"
    (lastPrims, fields) <- spanM (isPrim . snd) $ reverse vars
    let lastPrims' = map (\(x, _) -> [| toPrimitivePersistValue $(varE proxy) $(varE x) |]) $ reverse $ lastPrims
    let body = if null fields
        then [| return $ ($(listE $ [|toPrimitivePersistValue $(varE proxy) $(lift (cNum :: Int))|]:lastPrims')++) |]
        else do
          let go (m, f) (fname, t) = isPrim t >>= \isP -> if isP
              then return (m, [| (toPrimitivePersistValue $(varE proxy) $(varE fname):) |]:f)
              else newName "x" >>= \x -> return (bindS (varP x) [| toPersistValues $(varE fname) |]:m, varE x:f)
          (stmts, func) <- foldM go ([], []) fields        -- foldM puts reversed fields in normal order
          let nonPrimFields' = foldr1 (\a b -> [|$a . $b|]) func
          let result = if null lastPrims' then nonPrimFields' else [| $nonPrimFields' . ($(listE lastPrims')++) |]
          doE $ stmts ++ [noBindS [| return $ (toPrimitivePersistValue $(varE proxy) $(lift (cNum :: Int)):) . $result |]]
    let body' = [| phantomDb >>= $(lamE [varP proxy] body) |]
    clause [pat] (normalB body') []

  fromEntityPersistValues' <- do
      xs <- newName "xs"
      let failureBody = normalB [| (\a -> fail (failMessage a $(varE xs)) >> return (a, [])) undefined |]
      failureName <- newName "failure"
      let failure = match wildP (normalB $ varE failureName) []
      matches <- forM (zip [0..] (thConstructors def)) $ \(cNum, c) -> do
        let cNum' = conP 'PersistInt64 [litP $ integerL cNum]
        xs' <- newName "xs"
        (_, body) <- mkFromPersistValues failureName xs' (thConstrName c) (thConstrFields c)
        return $ match (infixP cNum' '(:) (varP xs')) (normalB $ return body) []
      let start = caseE (varE xs) $ matches ++ [failure]
      let failureFunc = funD failureName [clause [] failureBody []]
      funD 'fromEntityPersistValues [clause [varP xs] (normalB start) [failureFunc]]

  getUniques' <- let
    hasConstraints = not . null . thConstrUniques
    clauses = zipWith mkClause [0::Int ..] (thConstructors def)
    mkClause cNum cdef | not (hasConstraints cdef) = clause [wildP, conP (thConstrName cdef) pats] (normalB [| (cNum, []) |]) [] where
      pats = map (const wildP) $ thConstrFields cdef
    mkClause cNum cdef = do
      let allConstrainedFields = concatMap thUniqueFields $ thConstrUniques cdef
      vars <- mapM (\f -> newName "x" >>= \x -> return $ if thFieldName f `elem` allConstrainedFields then Just (x, thFieldType f) else Nothing) $ thConstrFields cdef
      let pat = conP (thConstrName cdef) $ map (maybe wildP (varP . fst)) vars
      proxy <- newName "p"
      let body = normalB $ [| (cNum, $(listE $ map (\(THUniqueDef uName _ fnames) -> [| (uName, $result) |] ) $ thConstrUniques cdef)) |]
          result = mkToPurePersistValues proxy (catMaybes vars)
      clause [varP proxy, pat] body []
    in funD 'getUniques clauses

  entityFieldChain' <- let
    thFieldNames = thConstructors def >>= thConstrFields
    clauses = map (\f -> mkChain f >>= \(fArg, body) -> clause [asP fArg $ conP (mkName $ thExprName f) []] (normalB body) []) thFieldNames
    mkChain f = do
        fArg <- newName "f"
        let nvar = [| (undefined :: Field v c a -> a) $(varE fArg) |]
            typ = mkType f nvar
        let body = [| (($(lift $ thDbFieldName f), $typ), []) |]
        return (fArg, body)
    in funD 'entityFieldChain clauses

  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  let decs = [key', autoKey', defaultKey', fields', entityDef', toEntityPersistValues', fromEntityPersistValues', getUniques', entityFieldChain']
  return $ [InstanceD context (AppT (ConT ''PersistEntity) entity) decs]

mkToPurePersistValues :: Name -> [(Name, Type)] -> Q Exp
mkToPurePersistValues proxy vars = do
  (lastPrims, fields) <- spanM (isPrim . snd) $ reverse vars
  let lastPrims' = map (\(x, _) -> [| toPrimitivePersistValue $(varE proxy) $(varE x) |]) $ reverse $ lastPrims
  if null fields
    then [| ($(listE lastPrims')++) |]
    else do
      let go (m, f) (fname, t) = isPrim t >>= \isP -> if isP
          then return (m, [| (toPrimitivePersistValue $(varE proxy) $(varE fname):) |]:f)
          else newName "x" >>= \x -> return (valD (varP x) (normalB [| toPurePersistValues $(varE proxy) $(varE fname) |]) []:m, varE x:f)
      (stmts, func) <- foldM go ([], []) fields        -- foldM puts reversed fields in normal order
      let nonPrimFields' = foldr1 (\a b -> [|$a . $b|]) func
      let result = if null lastPrims' then nonPrimFields' else [| $nonPrimFields' . ($(listE lastPrims')++) |]
      letE stmts result

mkEntityPersistFieldInstance :: THEntityDef -> Q [Dec]
mkEntityPersistFieldInstance def = do
  let types = map extractType $ thTypeParams def
  let entity = foldl AppT (ConT (thDataName def)) types
  
  persistName' <- do
    v <- newName "v"
    let mkLambda t = [|undefined :: $(forallT (thTypeParams def) (cxt []) [t| $(return entity) -> $(return t) |]) |]
    
    let paramNames = foldr1 (\p xs -> [| $p ++ [delim] ++ $xs |] ) $ map (\t -> [| persistName ($(mkLambda t) $(varE v)) |]) types
    let fullEntityName = case null types of
         True  -> [| $(stringE $ thDbEntityName def) |]
         False -> [| $(stringE $ thDbEntityName def) ++ [delim] ++ $(paramNames) |]
    let body = normalB $ fullEntityName
    let pat = if null types then wildP else varP v
    funD 'persistName $ [ clause [pat] body [] ]
  
  isOne <- isDefaultKeyOneColumn def
  let uniqInfo = either auto uniq $ getDefaultKey def where
      auto _ = Nothing
      uniq u = let name = mkName $ thUniqueKeyPhantomName u in Just $ (conT name, conE name)

  toPersistValues' <- do
    let body = normalB $ case uniqInfo of
         _ | isOne -> [| singleToPersistValue |]
         Just u    -> [| toPersistValuesUnique $(snd u) |]
         _         -> error "mkEntityPersistFieldInstance: key has no unique type"
    funD 'toPersistValues $ [ clause [] body [] ]

  fromPersistValue' <- do
    let body = normalB $ case uniqInfo of
         _ | isOne -> [| singleFromPersistValue |]
         Just u    -> [| fromPersistValuesUnique $(snd u) |]
         _         -> error "mkEntityPersistFieldInstance: key has no unique type"
    funD 'fromPersistValues $ [ clause [] body []]

  dbType' <- funD 'dbType $ [clause [] (normalB [| dbType . (undefined :: a -> DefaultKey a) |]) []]

  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  let decs = [persistName', toPersistValues', fromPersistValue', dbType']
  return $ [InstanceD context (AppT (ConT ''PersistField) entity) decs]

mkEntitySinglePersistFieldInstance :: THEntityDef -> Q [Dec]
mkEntitySinglePersistFieldInstance def = isDefaultKeyOneColumn def >>= \isOne ->
  if isOne
    then do
      toSinglePersistValue' <- funD 'toSinglePersistValue $ [ clause [] (normalB to) [] ]
      fromSinglePersistValue' <- funD 'fromSinglePersistValue $ [ clause [] (normalB from) []]
      let decs = [toSinglePersistValue', fromSinglePersistValue']
      return [InstanceD context (AppT (ConT ''SinglePersistField) entity) decs]
    else return [] where
    (to, from) = case getDefaultKey def of
      Left  _ -> ([| toSinglePersistValueAutoKey |], [| fromSinglePersistValueAutoKey |])
      Right k -> ([| toSinglePersistValueUnique $u |], [| fromSinglePersistValueUnique $u |]) where
        u = conE $ mkName $ thUniqueKeyPhantomName k
    types = map extractType $ thTypeParams def
    entity = foldl AppT (ConT (thDataName def)) types
    context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)

mkEntityNeverNullInstance :: THEntityDef -> Q [Dec]
mkEntityNeverNullInstance def = do
  let types = map extractType $ thTypeParams def
  let entity = foldl AppT (ConT (thDataName def)) types
  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  isOne <- isDefaultKeyOneColumn def
  return $ if isOne
    then [InstanceD context (AppT (ConT ''NeverNull) entity) []]
    else []

mkMigrateFunction :: String -> [THEntityDef] -> Q [Dec]
mkMigrateFunction name defs = do
  let (normal, polymorhpic) = partition (null . thTypeParams) defs
  forM_ polymorhpic $ \def -> reportWarning $ "Datatype " ++ show (thDataName def) ++ " will not be migrated automatically by function " ++ name ++ " because it has type parameters"
  let body = doE $ map (\def -> noBindS [| migrate (undefined :: $(conT $ thDataName def)) |]) normal
  sig <- sigD (mkName name) [t| PersistBackend m => Migration m |]
  func <- funD (mkName name) [clause [] (normalB body) []]
  return [sig, func]

isDefaultKeyOneColumn :: THEntityDef -> Q Bool
isDefaultKeyOneColumn def = either (const $ return True) checkUnique $ getDefaultKey def where
  checkUnique unique = if (length $ thUniqueKeyFields unique) == 1
    then isPrim $ thFieldType $ head $ thUniqueKeyFields unique
    else return False

getDefaultKey :: THEntityDef -> Either THAutoKeyDef THUniqueKeyDef
getDefaultKey def = case thAutoKey def of
  Just k | thAutoKeyIsDef k -> Left k
  _ -> Right $ head $ filter thUniqueKeyIsDef $ thUniqueKeys def

paramsContext :: [TyVarBndr] -> [THFieldDef] -> Cxt
paramsContext types fields = classPred ''PersistField params ++ classPred ''SinglePersistField maybys ++ classPred ''NeverNull maybys where
  classPred clazz = map (\t -> ClassP clazz [t])
  -- every type must be an instance of PersistField
  params = map extractType types
  -- all datatype fields also must be instances of PersistField
  -- if Maybe is applied to a type param, the param must be also an instance of NeverNull
  -- so that (Maybe param) is an instance of PersistField
  maybys = nub $ fields >>= insideMaybe . thFieldType

paramsPureContext :: [TyVarBndr] -> [THFieldDef] -> Q (Maybe Cxt)
paramsPureContext types fields = do
  let isValidType (VarT _) = return True
      isValidType t = isPrim t
  invalid <- filterM (liftM not . isValidType . thFieldType) fields
  return $ case invalid of
    [] -> Just $ classPred ''PurePersistField params ++ classPred ''PrimitivePersistField maybys ++ classPred ''NeverNull maybys where
          params = map extractType types
          classPred clazz = map (\t -> ClassP clazz [t])
          -- all datatype fields also must be instances of PersistField
          -- if Maybe is applied to a type param, the param must be also an instance of NeverNull
          -- so that (Maybe param) is an instance of PersistField
          maybys = nub $ fields >>= insideMaybe . thFieldType
    _  -> Nothing

extractType :: TyVarBndr -> Type
extractType (PlainTV name) = VarT name
extractType (KindedTV name _) = VarT name

#if MIN_VERSION_template_haskell(2, 7, 0)
#define isClassInstance isInstance
#endif

#if !MIN_VERSION_template_haskell(2, 8, 0)
reportWarning :: String -> Q ()
reportWarning = report False
#endif

isPrim :: Type -> Q Bool
-- we cannot use simply isClassInstance because it crashes on type vars and in this case
-- class PrimitivePersistField a
-- instance PrimitivePersistField Int
-- instance PrimitivePersistField a => Maybe a
-- it will consider (Maybe anytype) instance of PrimitivePersistField
isPrim t | hasFreeVars t = return False
isPrim t@(ConT _) = isClassInstance ''PrimitivePersistField [t]
--isPrim (AppT (ConT key) _)  | key == ''Key = return True
isPrim (AppT (AppT (ConT key) _) (AppT (AppT _ (ConT typ)) _))  | key == ''Key && typ == ''BackendSpecific  = return True
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
{-
mkType :: THFieldDef -> ExpQ -> ExpQ
mkType f nvar = case (thEmbeddedDef f, thDbTypeName f) of
  (Just e, _) -> [| applyEmbeddedDbTypeSettings $(lift e) (dbType $nvar) |]
  (_, Just name) -> [| DbOther $ OtherTypeDef $ const $(lift name) |]
  _ -> [| dbType $nvar |]
-}
mkType :: THFieldDef -> ExpQ -> ExpQ
mkType f nvar = case thDbTypeName f of
  Just name -> [| DbOther $ OtherTypeDef $ const $(lift name) |]
  Nothing -> t3 where
    t1 = [| dbType $nvar |]
    t2 = case (thFieldOnDelete f, thFieldOnUpdate f) of
      (Nothing, Nothing) -> t1
      (onDel, onUpd) -> [| applyReferencesSettings $(lift onDel) $(lift onUpd) $t1 |]
    t3 = case thEmbeddedDef f of
      Nothing -> t2
      Just emb -> [| applyEmbeddedDbTypeSettings $(lift emb) $t2 |]
  
  {-
  
  applyEmbeddedDbTypeSettings
  (1, 2) Embedded
  Key Entity Embedded
  Entity PersistEntity
  -}
