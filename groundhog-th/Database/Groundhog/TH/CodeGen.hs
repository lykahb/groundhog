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
  , mkPrimitivePersistFieldInstance
  , mkPrimitivePrimitivePersistFieldInstance
  , mkMigrateFunction
  ) where
  
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.TH.Settings
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Control.Arrow (second)
import Control.Monad (liftM, liftM2, forM, forM_, foldM, filterM, replicateM)
import Data.Either (lefts, rights)
import Data.List (findIndex, nub, partition)
import Data.Maybe (catMaybes, mapMaybe)

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
    proxy <- newName "p"
    let mkField fNum f = do
        a <- newName "a"
        let fname = thDbFieldName f
            nvar = if hasFreeVars (thFieldType f)
              then let pat = conP (thEmbeddedConstructorName def) $ replicate fNum wildP ++ [varP a] ++ replicate (length (thEmbeddedFields def) - fNum - 1) wildP
                   in caseE (varE v) $ [match pat (normalB $ varE a) []]
              else [| undefined :: $(return $ thFieldType f) |]
            typ = mkType f proxy nvar
        [| (fname, $typ) |]
    let pat = if null $ thEmbeddedTypeParams def then wildP else varP v
    funD 'dbType $ [ clause [varP proxy, pat] (normalB [| DbEmbedded (EmbeddedDef False $(listE $ zipWith mkField [0..] $ thEmbeddedFields def)) Nothing |]) [] ]

  let context = paramsContext (thEmbeddedTypeParams def) (thEmbeddedFields def)
  let decs = [persistName', toPersistValues', fromPersistValues', dbType']
  return $ [InstanceD context (AppT (ConT ''PersistField) embedded) decs]

mkFromPersistValues :: Name -> Name -> Name -> [THFieldDef] -> Q (Bool, Exp)
mkFromPersistValues failureName values constrName fieldDefs = do
  proxy <- newName "p"
  allVars <- mapM (\f -> newName "x" >>= \fname -> return (fname, thFieldType f)) fieldDefs
  let failure = match wildP (normalB $ varE failureName) []
      mkArg (fname, t) = do
        isP <- isPrim t
        if isP
          then [| fromPrimitivePersistValue $(varE proxy) $(varE fname) |]
          else varE fname
      result = foldl (\a f -> appE a $ mkArg f) (conE constrName) allVars
      goField xs vars = do
        (fields, rest) <- spanM (liftM not . isPrim . snd) vars
        xss <- liftM (xs:) $ mapM (const $ newName "xs") fields
        let f oldXs newXs (fname, _) = bindS (conP '(,) [varP fname, varP newXs]) [| fromPersistValues $(varE oldXs) |]
        let stmts = zipWith3 f xss (tail xss) fields
        expr <- goPrim (last xss) rest
        return $ doE $ stmts ++ [noBindS expr]
      goPrim xs vars = do
        xs' <- newName "xs"
        (prim, rest) <- spanM (isPrim . snd) vars
        body' <- case rest of
          [] -> return [| return ($result, $(varE xs')) |]
          _  -> goField xs' rest
        let m = match (foldr (\(fname, _) p -> infixP (varP fname) '(:) p) (varP xs') prim) (normalB body') []
        return $ if null prim
          then caseE (varE xs) [m]
          else caseE (varE xs) [m, failure]
  body <- goPrim values allVars
  anyPrim <- liftM or $ mapM (isPrim . snd) allVars
  body' <- if anyPrim then [| phantomDb >>= $(lamE [varP proxy] body) |] else body
  return (anyPrim, body')

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
  Just _ -> do
    let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
    keyType <- [t| Key $(return entity) BackendSpecific |]
    
    persistName' <- do
      a <- newName "a"
      let body = [| "Key" ++ [delim] ++ persistName ((undefined :: Key v u -> v) $(varE a)) |]
      funD 'persistName [clause [varP a] (normalB body) []]
    toPersistValues' <- funD 'toPersistValues [clause [] (normalB [| primToPersistValue |]) []]
    fromPersistValues' <- funD 'fromPersistValues [clause [] (normalB [| primFromPersistValue |]) []]
    dbType' <- do
      proxy <- newName "p"
      a <- newName "a"
      let e = [| entityDef $(varE proxy) ((undefined :: Key v a -> v) $(varE a)) |]
          body = [| DbTypePrimitive (getAutoKeyType $(varE proxy)) False Nothing (Just (Left ($e, Nothing), Nothing, Nothing)) |]
      funD 'dbType [clause [varP proxy, varP a] (normalB body) []]
    
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
      let body = [| toPrimitivePersistValue $(varE proxy) $ ((fromPrimitivePersistValue :: DbDescriptor db => proxy db -> PersistValue -> AutoKeyType db) $(varE proxy)) $(varE x) |]
      funD 'toPrimitivePersistValue [clause [varP proxy, conP conName [varP x]] (normalB body) []]
    fromPrim' <- funD 'fromPrimitivePersistValue [clause [wildP] (normalB $ conE conName) []]
    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    let decs = [toPrim', fromPrim']
    sequence [ return $ InstanceD context (AppT (ConT ''PrimitivePersistField) keyType) decs
             , return $ InstanceD context (AppT (ConT ''NeverNull) keyType) []
             , mkDefaultPurePersistFieldInstance context keyType
             , mkDefaultSinglePersistFieldInstance context keyType]
  _ -> return []

mkDefaultPurePersistFieldInstance :: Cxt -> Type -> Q Dec
mkDefaultPurePersistFieldInstance context typ = do
  toPurePersistValues' <- funD 'toPurePersistValues [clause [] (normalB [| primToPurePersistValues |]) []]
  fromPurePersistValues' <- funD 'fromPurePersistValues [clause [] (normalB [| primFromPurePersistValues |]) []]
  let decs = [toPurePersistValues', fromPurePersistValues']
  return $ InstanceD context (AppT (ConT ''PurePersistField) typ) decs

mkDefaultSinglePersistFieldInstance :: Cxt -> Type -> Q Dec
mkDefaultSinglePersistFieldInstance context typ = do
  toSinglePersistValue' <- funD 'toSinglePersistValue [clause [] (normalB [| primToSinglePersistValue |]) []]
  fromSinglePersistValue' <- funD 'fromSinglePersistValue [clause [] (normalB [| primFromSinglePersistValue |]) []]
  let decs = [toSinglePersistValue', fromSinglePersistValue']
  return $ InstanceD context (AppT (ConT ''SinglePersistField) typ) decs

mkUniqueKeysIsUniqueInstances :: THEntityDef -> Q [Dec]
mkUniqueKeysIsUniqueInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  let constr = head $ thConstructors def
  forM (thUniqueKeys def) $ \unique -> do
    uniqKeyType <- [t| Key $(return entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique)) |]
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
    return $ InstanceD context (AppT (ConT ''IsUniqueKey) uniqKeyType) [extractUnique', uniqueNum']

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
      proxy <- newName "p"
      let mkField f = do
          let fname = thDbFieldName f
              nvar = [| undefined :: $(return $ thFieldType f) |]
              typ = mkType f proxy nvar
          [| (fname, $typ) |]
      let embedded = [| EmbeddedDef False $(listE $ map mkField $ thUniqueKeyFields unique) |]
          e = [| entityDef $(varE proxy) ((undefined :: Key v a -> v) $(varE a)) |]
          body = [| DbEmbedded $embedded (Just (Left ($e, Just $(lift $ thUniqueKeyName unique)), Nothing, Nothing)) |]
      funD 'dbType [clause [varP proxy, varP a] (normalB body) []]
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
    isUniquePrim <- case thUniqueKeyFields unique of
      [uniq] -> isPrim $ thFieldType uniq
      _      -> return False
    if isUniquePrim
      then do
        proxy <- newName "p"
        x <- newName "x"
        toPrim' <- do
          funD 'toPrimitivePersistValue [clause [varP proxy, conP conName [varP x]] (normalB [| toPrimitivePersistValue $(varE proxy) $(varE x) |]) []]
        fromPrim' <- funD 'fromPrimitivePersistValue [clause [varP proxy, varP x] (normalB [| $(conE conName) (fromPrimitivePersistValue $(varE proxy) $(varE x)) |]) []]
        let decs = [toPrim', fromPrim']
        sequence [ return $ InstanceD context (AppT (ConT ''PrimitivePersistField) uniqKeyType) decs
                 , return $ InstanceD context (AppT (ConT ''NeverNull) uniqKeyType) decs
                 , mkDefaultPurePersistFieldInstance context uniqKeyType
                 , mkDefaultSinglePersistFieldInstance context uniqKeyType]
      else mkPurePersistFieldInstance uniqKeyType conName (thUniqueKeyFields unique) context

mkKeyEqShowInstances :: THEntityDef -> Q [Dec]
mkKeyEqShowInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  let keysInfo = maybe [] (\k -> [(thAutoKeyConstrName k, 1)]) (thAutoKey def)
               ++ map (\k -> (thUniqueKeyConstrName k, length $ thUniqueKeyFields k)) (thUniqueKeys def)

  showsPrec' <- let
    mkClause (cName, fieldsNum) = do
      p <- newName "p"
      fields <- replicateM fieldsNum (newName "x")
      let pat = conP (mkName cName) $ map varP fields
          showC = [| showString $(lift $ cName ++ " ") |]
          showArgs = foldr1 (\a b -> [| $a . showString " " . $b |]) $ map (\a -> [| showsPrec 11 $(varE a) |]) fields
          body = [| showParen ($(varE p) >= (11 :: Int)) ($showC . $showArgs) |]
      clause [varP p, pat] (normalB body) []
    in funD 'showsPrec $ map mkClause keysInfo
    
  eq' <- let
    mkClause (cName, fieldsNum) = do
      let fields = replicateM fieldsNum (newName "x")
      (fields1, fields2) <- liftM2 (,) fields fields
      let mkPat = conP (mkName cName) . map varP
          body = foldr1 (\e1 e2 -> [| $e1 && $e2 |]) $ zipWith (\n1 n2 -> [| $(varE n1) == $(varE n2) |]) fields1 fields2
      clause [mkPat fields1, mkPat fields2] (normalB body) []
    clauses = map mkClause keysInfo
    noMatch = if length clauses > 1 then [clause [wildP, wildP] (normalB [| False |]) []] else []
    in funD '(==) $ clauses ++ noMatch

  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  typ <- [t| Key $(return entity) $(newName "a" >>= varT) |]
  return $ if null keysInfo
    then []
    else [InstanceD context (AppT (ConT ''Eq) typ) [eq'], InstanceD context (AppT (ConT ''Show) typ) [showsPrec']]

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
    return $ DataD [] name [PlainTV v] [constr] []
  
mkEntityPhantomConstructorInstances :: THEntityDef -> Q [Dec]
mkEntityPhantomConstructorInstances def = sequence $ zipWith f [0..] $ thConstructors def where
  f :: Int -> THConstructorDef -> Q Dec
  f cNum c = instanceD (cxt []) (appT (conT ''Constructor) (conT $ mkName $ thPhantomConstrName c)) [phantomConstrNum'] where
    phantomConstrNum' = funD 'phantomConstrNum [clause [wildP] (normalB $ [| cNum |]) []]

mkEntityUniqueKeysPhantoms :: THEntityDef -> Q [Dec]
mkEntityUniqueKeysPhantoms def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  fmap concat $ forM (thUniqueKeys def) $ \u -> do
    exists <- lookupTypeName $ thUniqueKeyPhantomName u
    if exists == Nothing
      then do
        v <- newName "v"
        let name = mkName $ thUniqueKeyPhantomName u
        phantom <- [t| UniqueMarker $(return entity) |]
        let constr = ForallC (thTypeParams def) [EqualP (VarT v) phantom] $ NormalC name []
        return [DataD [] name [PlainTV v] [constr] []]
      else return []
    
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
      uniqType <- [t| Unique $(conT $ mkName $ thUniqueKeyPhantomName unique) |]
      let cDef = head $ thConstructors def
          uniqFieldNames = lefts $ thUniqueFields $ findOne "unique" thUniqueName (thUniqueKeyName unique) $ thConstrUniques cDef
          uniqFields = concat $ flip map uniqFieldNames $ \name -> (filter ((== name) . thFieldName) $ thConstrFields cDef)
          uniqFields' = map (\f -> (NotStrict, thFieldType f)) uniqFields
      return $ ForallC [] [EqualP (VarT uParam) uniqType] $ NormalC (mkName $ thUniqueKeyConstrName unique) uniqFields'
    return $ DataInstD [] ''Key [entity, VarT uParam] (autoKey ++ uniques) []

  autoKey' <- do
    autoType <- case thAutoKey def of
      Nothing -> [t| () |]
      Just _ -> [t| Key $(return entity) BackendSpecific |]
    return $ mkTySynInstD ''AutoKey [entity] autoType
    
  defaultKey' <- do
    typ <- case thAutoKey def of
      Just k | thAutoKeyIsDef k -> [t| Key $(return entity) BackendSpecific |]
      _ -> case filter thUniqueKeyIsDef $ thUniqueKeys def of
        [unique] -> [t| Key $(return entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique)) |]
        _ -> [t| () |]
    return $ mkTySynInstD ''DefaultKey [entity] typ

  isSumType' <- do
    let isSumType = ConT $ if length (thConstructors def) == 1
          then ''HFalse
          else ''HTrue
    return $ mkTySynInstD ''IsSumType [entity] isSumType
  
  fields' <- do
    cParam <- newName "c"
    fParam <- newName "f"
    let mkField name field = ForallC [] [EqualP (VarT cParam) (ConT name), EqualP (VarT fParam) (thFieldType field)] $ NormalC (mkName $ thExprName field) []
    let f cdef = map (mkField $ mkName $ thPhantomConstrName cdef) $ thConstrFields cdef
    let constrs = concatMap f $ thConstructors def
    return $ DataInstD [] ''Field [entity, VarT cParam, VarT fParam] constrs []
    
  entityDef' <- do
    v <- newName "v"
    proxy <- newName "p"
    let mkLambda t = [|undefined :: $(forallT (thTypeParams def) (cxt []) [t| $(return entity) -> $(return t) |]) |]
        types = map extractType $ thTypeParams def
        typeParams' = listE $ map (\t -> [| dbType $(varE proxy) ($(mkLambda t) $(varE v)) |]) types
        mkField c fNum f = do
          a <- newName "a"
          let fname = thDbFieldName f
              nvar = if hasFreeVars (thFieldType f)
                then let pat = conP (thConstrName c) $ replicate fNum wildP ++ [varP a] ++ replicate (length (thConstrFields c) - fNum - 1) wildP
                         wildClause = if length (thConstructors def) > 1 then [match wildP (normalB [| undefined |]) []] else []
                     in caseE (varE v) $ [match pat (normalB $ varE a) []] ++ wildClause
                else [| undefined :: $(return $ thFieldType f) |]
              typ = mkType f proxy nvar
          [| (fname, $typ) |]
        constrs = listE $ map mkConstructorDef $ thConstructors def
        mkConstructorDef c@(THConstructorDef _ _ name keyName params conss) = [| ConstructorDef name keyName $(listE $ map snd fields) $(listE $ map mkConstraint conss) |] where
          fields = zipWith (\i f -> (thFieldName f, mkField c i f)) [0..] params
          mkConstraint (THUniqueDef uName uType uFields) = [| UniqueDef (Just uName) uType $(listE $ map getField uFields) |]
          getField (Left fName) = [| Left $(snd $ findOne "field" fst fName fields) |]
          getField (Right expr) = [| Right expr |]
    
        paramNames = foldr1 (\p xs -> [| $p ++ [delim] ++ $xs |] ) $ map (\t -> [| persistName ($(mkLambda t) $(varE v)) |]) types
        fullEntityName = case null types of
         True  -> [| $(stringE $ thDbEntityName def) |]
         False -> [| $(stringE $ thDbEntityName def) ++ [delim] ++ $(paramNames) |]

        body = normalB [| EntityDef $fullEntityName $(lift $ thEntitySchema def) $typeParams' $constrs |]
        entityPat = if null $ thTypeParams def then wildP else varP v
    funD 'entityDef $ [ clause [varP proxy, entityPat] body [] ]

  toEntityPersistValues' <- liftM (FunD 'toEntityPersistValues) $ forM (zip [0 :: Int ..] $ thConstructors def) $ \(cNum, c) -> do
    vars <- mapM (\f -> newName "x" >>= \fname -> return (fname, thFieldType f)) $ thConstrFields c
    let pat = conP (thConstrName c) $ map (varP . fst) vars
    proxy <- newName "p"
    (lastPrims, fields) <- spanM (isPrim . snd) $ reverse vars
    let lastPrims' = map (\(x, _) -> [| toPrimitivePersistValue $(varE proxy) $(varE x) |]) $ reverse $ lastPrims
    let body = if null fields
        then [| return $ ($(listE $ [|toPrimitivePersistValue $(varE proxy) ($(lift cNum) :: Int) |]:lastPrims')++) |]
        else do
          let go (m, f) (fname, t) = isPrim t >>= \isP -> if isP
              then return (m, [| (toPrimitivePersistValue $(varE proxy) $(varE fname):) |]:f)
              else newName "x" >>= \x -> return (bindS (varP x) [| toPersistValues $(varE fname) |]:m, varE x:f)
          (stmts, func) <- foldM go ([], []) fields        -- foldM puts reversed fields in normal order
          let nonPrimFields' = foldr1 (\a b -> [|$a . $b|]) func
          let result = if null lastPrims' then nonPrimFields' else [| $nonPrimFields' . ($(listE lastPrims')++) |]
          doE $ stmts ++ [noBindS [| return $ (toPrimitivePersistValue $(varE proxy) ($(lift cNum) :: Int):) . $result |]]
    let body' = [| phantomDb >>= $(lamE [varP proxy] body) |]
    clause [pat] (normalB body') []

  fromEntityPersistValues' <- do
      xs <- newName "xs"
      let failureBody = normalB [| (\a -> phantomDb >>= \proxy -> fail (failMessageNamed (entityName $ entityDef proxy a) $(varE xs)) >> return (a, [])) undefined |]
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
      let allConstrainedFields = lefts $ concatMap thUniqueFields $ thConstrUniques cdef
      vars <- mapM (\f -> newName "x" >>= \x -> return $ if thFieldName f `elem` allConstrainedFields then Just (x, f) else Nothing) $ thConstrFields cdef
      proxy <- newName "p"
      let pat = conP (thConstrName cdef) $ map (maybe wildP (varP . fst)) vars
          body = normalB $ [| (cNum, $(listE $ mapMaybe mkUnique $ thConstrUniques cdef)) |]
          mkUnique (THUniqueDef uName _ fnames) = if null $ rights fnames
            then let
              -- find corresponding field from vars
              uFields = map (\f -> findOne "field" (thFieldName . snd) f $ catMaybes vars) $ lefts fnames
              result = mkToPurePersistValues proxy $ map (second thFieldType) uFields
              in Just [| (uName, $result) |]
            else Nothing
      clause [varP proxy, pat] body []
    in funD 'getUniques clauses

  entityFieldChain' <- let
    thFieldNames = thConstructors def >>= thConstrFields
    clauses = map mkClause thFieldNames
    mkClause f = do
        fArg <- newName "f"
        proxy <- newName "p"
        let nvar = [| (undefined :: Field v c a -> a) $(varE fArg) |]
            typ = mkType f proxy nvar
            body = [| (($(lift $ thDbFieldName f), $typ), []) |]
        clause [varP proxy, asP fArg $ conP (mkName $ thExprName f) []] (normalB body) []
    clauses' = if null clauses then [clause [wildP] (normalB [| undefined |]) []] else clauses
    in funD 'entityFieldChain clauses'

  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  let decs = [key', autoKey', defaultKey', isSumType', fields', entityDef', toEntityPersistValues', fromEntityPersistValues', getUniques', entityFieldChain']
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
mkEntityPersistFieldInstance def = case getDefaultKey def of
  Just defaultKey -> do
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
    let uniqInfo = either auto uniq defaultKey where
        auto _ = Nothing
        uniq u = let name = mkName $ thUniqueKeyPhantomName u in Just $ (conT name, conE name)

    toPersistValues' <- do
      let body = normalB $ case uniqInfo of
           _ | isOne -> [| singleToPersistValue |]
           Just u    -> [| toPersistValuesUnique $(snd u) |]
           _         -> error "mkEntityPersistFieldInstance: key has no unique type"
      funD 'toPersistValues $ [ clause [] body [] ]

    fromPersistValues' <- do
      let body = normalB $ case uniqInfo of
           _ | isOne -> [| singleFromPersistValue |]
           Just u    -> [| fromPersistValuesUnique $(snd u) |]
           _         -> error "mkEntityPersistFieldInstance: key has no unique type"
      funD 'fromPersistValues $ [ clause [] body []]

    dbType' <- do
      proxy <- newName "p"
      let body = [| dbType $(varE proxy) . (undefined :: a -> DefaultKey a) |]
      funD 'dbType $ [clause [varP proxy] (normalB body) []]

    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    let decs = [persistName', toPersistValues', fromPersistValues', dbType']
    return $ [InstanceD context (AppT (ConT ''PersistField) entity) decs]
  Nothing -> return []

mkEntitySinglePersistFieldInstance :: THEntityDef -> Q [Dec]
mkEntitySinglePersistFieldInstance def = isDefaultKeyOneColumn def >>= \isOne -> case getDefaultKey def of
  Just defaultKey | isOne -> do
    let types = map extractType $ thTypeParams def
        entity = foldl AppT (ConT (thDataName def)) types
        context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)

        (to, from) = case defaultKey of
          Left  _ -> ([| toSinglePersistValueAutoKey |], [| fromSinglePersistValueAutoKey |])
          Right k -> ([| toSinglePersistValueUnique $u |], [| fromSinglePersistValueUnique $u |]) where
            u = conE $ mkName $ thUniqueKeyPhantomName k

    toSinglePersistValue' <- funD 'toSinglePersistValue $ [ clause [] (normalB to) [] ]
    fromSinglePersistValue' <- funD 'fromSinglePersistValue $ [ clause [] (normalB from) []]
    let decs = [toSinglePersistValue', fromSinglePersistValue']
    return [InstanceD context (AppT (ConT ''SinglePersistField) entity) decs]
  _ -> return []

mkEntityNeverNullInstance :: THEntityDef -> Q [Dec]
mkEntityNeverNullInstance def = do
  let types = map extractType $ thTypeParams def
      entity = foldl AppT (ConT (thDataName def)) types
      context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  isOne <- isDefaultKeyOneColumn def
  return $ if isOne
    then [InstanceD context (AppT (ConT ''NeverNull) entity) []]
    else []

mkPrimitivePersistFieldInstance :: THPrimitiveDef -> Q [Dec]
mkPrimitivePersistFieldInstance def = do
  let prim = ConT (thPrimitiveName def)
  persistName' <- do
    let body = normalB $ stringE $ nameBase $ thPrimitiveName def
    funD 'persistName $ [ clause [wildP] body [] ]
  fromPersistValues' <- funD 'fromPersistValues [clause [] (normalB [| primFromPersistValue |]) []]
  toPersistValues' <- funD 'toPersistValues [clause [] (normalB [| primToPersistValue |]) []]
  dbType' <- do
    let typ = if thPrimitiveStringEnumRepresentation def
          then [| DbTypePrimitive DbString False Nothing Nothing |]
          else [| DbTypePrimitive DbInt32  False Nothing Nothing |]
    funD 'dbType $ [ clause [wildP, wildP] (normalB typ) [] ]
  let decs = [persistName', toPersistValues', fromPersistValues', dbType']
  return [ InstanceD [] (AppT (ConT ''PersistField) prim) decs
         , InstanceD [] (AppT (ConT ''NeverNull) prim) []
         ]

mkPrimitivePrimitivePersistFieldInstance :: THPrimitiveDef -> Q [Dec]
mkPrimitivePrimitivePersistFieldInstance def = do
  let prim = ConT (thPrimitiveName def)
  toPrim' <- do
    proxy <- newName "p"
    x <- newName "x"
    let value = if thPrimitiveStringEnumRepresentation def
          then [| show $(varE x) |]
          else [| fromEnum $(varE x) |]
        body = [| toPrimitivePersistValue $(varE proxy) $value |]
    funD 'toPrimitivePersistValue [clause [varP proxy, varP x] (normalB body) []]
  fromPrim' <- do
    proxy <- newName "p"
    x <- newName "x"
    let value = [| fromPrimitivePersistValue $(varE proxy) $(varE x) |]
        body = if thPrimitiveStringEnumRepresentation def
          then [| read $value |]
          else [| toEnum $value |]
    funD 'fromPrimitivePersistValue [clause [varP proxy, varP x] (normalB body) []]
  let context = []
  let decs = [toPrim', fromPrim']
  sequence $ [return $ InstanceD context (AppT (ConT ''PrimitivePersistField) prim) decs
           , mkDefaultPurePersistFieldInstance context prim
           , mkDefaultSinglePersistFieldInstance context prim]

mkMigrateFunction :: String -> [THEntityDef] -> Q [Dec]
mkMigrateFunction name defs = do
  let (normal, polymorhpic) = partition (null . thTypeParams) defs
  forM_ polymorhpic $ \def -> reportWarning $ "Datatype " ++ show (thDataName def) ++ " will not be migrated automatically by function " ++ name ++ " because it has type parameters"
  let body = doE $ map (\def -> noBindS [| migrate (undefined :: $(conT $ thDataName def)) |]) normal
  sig <- sigD (mkName name) [t| PersistBackend m => Migration m |]
  func <- funD (mkName name) [clause [] (normalB body) []]
  return [sig, func]

isDefaultKeyOneColumn :: THEntityDef -> Q Bool
isDefaultKeyOneColumn def = case getDefaultKey def of
  Just (Left _) -> return True
  Just (Right unique) | (length $ thUniqueKeyFields unique) == 1 ->
    isPrim $ thFieldType $ head $ thUniqueKeyFields unique
  _ -> return False

getDefaultKey :: THEntityDef -> Maybe (Either THAutoKeyDef THUniqueKeyDef)
getDefaultKey def = case thAutoKey def of
  Just k | thAutoKeyIsDef k -> Just $ Left k
  _ -> case filter thUniqueKeyIsDef $ thUniqueKeys def of
    [] -> Nothing
    (u:_) -> Just $ Right u

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

mkType :: THFieldDef -> Name -> ExpQ -> ExpQ
mkType THFieldDef{..} proxy nvar = t2 where
  psField = PSFieldDef thFieldName (Just thDbFieldName) thDbTypeName (Just thExprName) thEmbeddedDef thDefaultValue thReferenceParent
  t1 = [| dbType $(varE proxy) $nvar |]
  -- if there are any type settings, apply them in runtime
  t2 = case (thDbTypeName, thEmbeddedDef, thDefaultValue, thReferenceParent) of
    (Nothing, Nothing, Nothing, Nothing) -> t1
    _ -> [| applyDbTypeSettings $(lift psField) $t1 |]

mkTySynInstD :: Name -> [Type] -> Type -> Dec
mkTySynInstD name ts t =
#if MIN_VERSION_template_haskell(2, 9, 0)
  TySynInstD name $ TySynEqn ts t
#else
  TySynInstD name ts t
#endif
