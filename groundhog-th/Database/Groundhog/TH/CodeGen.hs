{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Groundhog.TH.CodeGen
  ( mkEmbeddedPersistFieldInstance,
    mkEmbeddedPurePersistFieldInstance,
    mkEmbeddedInstance,
    mkEntityPhantomConstructors,
    mkEntityPhantomConstructorInstances,
    mkEntityUniqueKeysPhantoms,
    mkAutoKeyPersistFieldInstance,
    mkAutoKeyPrimitivePersistFieldInstance,
    mkUniqueKeysIsUniqueInstances,
    mkUniqueKeysEmbeddedInstances,
    mkUniqueKeysPersistFieldInstances,
    mkUniqueKeysPrimitiveOrPurePersistFieldInstances,
    mkKeyEqShowInstances,
    mkEntityPersistFieldInstance,
    mkEntitySinglePersistFieldInstance,
    mkPersistEntityInstance,
    mkEntityNeverNullInstance,
    mkPrimitivePersistFieldInstance,
    mkPrimitivePrimitivePersistFieldInstance,
    mkMigrateFunction,
  )
where

import Control.Arrow (first)
import Control.Monad (filterM, forM, forM_, liftM2, replicateM, zipWithM)
import Data.Either (lefts, rights)
import Data.List (findIndex, nub, partition)
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.TH.Settings
import qualified GHC.Read as R
import Language.Haskell.TH hiding (TyVarBndr)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (Lift (..))
import qualified Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as R

mkEmbeddedPersistFieldInstance :: THEmbeddedDef -> Q [Dec]
mkEmbeddedPersistFieldInstance def = do
  let types = map extractType $ thEmbeddedTypeParams def
  let embedded = foldl AppT (ConT (thEmbeddedName def)) types

  persistName' <- do
    v <- newName "v"
    let mkLambda t = [|undefined :: $(pure embedded) -> $(pure t)|]
    let paramNames = foldr1 (\p xs -> [|$p ++ [delim] ++ $xs|]) $ map (\t -> [|persistName ($(mkLambda t) $(varE v))|]) types
    let fullEmbeddedName =
          if null types
            then [|$(stringE $ thDbEmbeddedName def)|]
            else [|$(stringE $ thDbEmbeddedName def) ++ [delim] ++ $(paramNames)|]
    let body = normalB fullEmbeddedName
    let pat = if null types then wildP else varP v
    funD 'persistName [clause [pat] body []]

  toPersistValues' <- do
    (pat, body) <- mkToPersistValues (thEmbeddedConstructorName def) (thEmbeddedFields def) id
    funD 'toPersistValues [clause [pat] (normalB body) []]

  fromPersistValues' <- do
    xs <- newName "xs"
    failureName <- newName "failure"
    (isFailureUsed, body) <- mkFromPersistValues failureName xs (thEmbeddedConstructorName def) (thEmbeddedFields def)
    let failureBody = normalB [|(\a -> fail (failMessage a $(varE xs)) >> pure (a, [])) undefined|]
        failureFunc = funD failureName [clause [] failureBody []]
        locals = if isFailureUsed then [failureFunc] else []
    funD 'fromPersistValues [clause [varP xs] (normalB $ pure body) locals]

  dbType' <- do
    v <- newName "v"
    proxy <- newName "p"
    let mkField fNum f = do
          a <- newName "a"
          let fname = thDbFieldName f
              nvar =
                if hasFreeVars (thFieldType f)
                  then
                    let pat = conP (thEmbeddedConstructorName def) $ replicate fNum wildP ++ [varP a] ++ replicate (length (thEmbeddedFields def) - fNum - 1) wildP
                     in caseE (varE v) [match pat (normalB $ varE a) []]
                  else [|undefined :: $(pure $ thFieldType f)|]
              typ = mkType f proxy nvar
          [|(fname, $typ)|]
    let pat = if null $ thEmbeddedTypeParams def then wildP else varP v
    funD 'dbType [clause [varP proxy, pat] (normalB [|DbEmbedded (EmbeddedDef False $(listE $ zipWith mkField [0 ..] $ thEmbeddedFields def)) Nothing|]) []]

  let context = paramsContext (thEmbeddedTypeParams def) (thEmbeddedFields def)
  let decs = [persistName', toPersistValues', fromPersistValues', dbType']
  pure [instanceD' context (AppT (ConT ''PersistField) embedded) decs]

mkToPersistValues :: Name -> [THFieldDef] -> (ExpQ -> ExpQ) -> Q (PatQ, ExpQ)
mkToPersistValues constrName fieldDefs processResult = do
  patVars <- mapM (const $ newName "x") fieldDefs
  let processField fName fDef = do
        isP <- isPrim (thFieldType fDef)
        let field = maybe id (\convName x -> [|fst $(varE convName) $ $x|]) (thFieldConverter fDef) (varE fName)
        if isP
          then pure (Nothing, [|(toPrimitivePersistValue $field :)|])
          else newName "x" >>= \x -> pure (Just $ bindS (varP x) [|toPersistValues $field|], varE x)
  (binds, funcs) <- first catMaybes . unzip <$> zipWithM processField patVars fieldDefs
  let pat = conP constrName $ map varP patVars
      result = [|pure $(processResult $ if null funcs then [|id|] else foldr1 (\a b -> [|$a . $b|]) funcs)|]
      body = if null binds then result else doE $ binds ++ [noBindS result]
  pure (pat, body)

mkFromPersistValues :: Name -> Name -> Name -> [THFieldDef] -> Q (Bool, Exp)
mkFromPersistValues failureName values constrName fieldDefs = do
  patVars <- mapM (const $ newName "x") fieldDefs
  let failure = match wildP (normalB $ varE failureName) []
      mkArg (fName, fDef) = do
        isP <- isPrim $ thFieldType fDef
        let x =
              if isP
                then [|fromPrimitivePersistValue $(varE fName)|]
                else varE fName
        maybe x (\convName -> [|snd $(varE convName) $ $x|]) $ thFieldConverter fDef
      result = foldl (\func f -> appE func $ mkArg f) (conE constrName) $ zip patVars fieldDefs
      goField xs vars = do
        (fields, rest) <- spanM (fmap not . isPrim . thFieldType . snd) vars
        xss <- (xs :) <$> mapM (const $ newName "xs") fields
        let f oldXs newXs (fname, _) = bindS (conP '(,) [varP fname, varP newXs]) [|fromPersistValues $(varE oldXs)|]
            stmts = zipWith3 f xss (tail xss) fields
            expr = goPrim (last xss) rest
        doE $ stmts ++ [noBindS expr]
      goPrim xs vars = do
        xs' <- newName "xs"
        (prims, rest) <- spanM (isPrim . thFieldType . snd) vars
        let body' = case rest of
              [] -> [|pure ($result, $(varE xs'))|]
              _ -> goField xs' rest
            m = match (foldr (\(fName, _) p -> infixP (varP fName) '(:) p) (varP xs') prims) (normalB body') []
        if null prims
          then caseE (varE xs) [m]
          else caseE (varE xs) [m, failure]
  body <- goPrim values $ zip patVars fieldDefs
  anyPrim <- or <$> mapM (isPrim . thFieldType) fieldDefs
  pure (anyPrim, body)

mkPurePersistFieldInstance :: Type -> Name -> [THFieldDef] -> Cxt -> Q [Dec]
mkPurePersistFieldInstance dataType cName fieldDefs context = do
  toPurePersistValues' <- do
    vars <- mapM (const $ newName "x") fieldDefs
    let pat = conP cName $ map varP vars
        body = mkToPurePersistValues $ zip vars fieldDefs
    funD 'toPurePersistValues [clause [pat] (normalB body) []]

  fromPurePersistValues' <-
    let goField xs vars result failure = do
          (fields, rest) <- spanM (fmap not . isPrim . thFieldType . snd) vars
          xss <- (xs :) <$> mapM (const $ newName "xs") fields
          let f oldXs newXs (fName, _) = valD (conP '(,) [varP fName, varP newXs]) (normalB [|fromPurePersistValues $(varE oldXs)|]) []
          let stmts = zipWith3 f xss (tail xss) fields
          (isFailureUsed, expr) <- goPrim (last xss) rest result failure
          pure (isFailureUsed, letE stmts expr)
        goPrim xs vars result failure = do
          xs' <- newName "xs"
          (prims, rest) <- spanM (isPrim . thFieldType . snd) vars
          (isFailureUsed, body') <- case rest of
            [] -> pure (False, [|($result, $(varE xs'))|])
            _ -> goField xs' rest result failure
          let m = match (foldr (\(fName, _) p -> infixP (varP fName) '(:) p) (varP xs') prims) (normalB body') []
          pure $
            if null prims
              then (isFailureUsed, caseE (varE xs) [m])
              else (True, caseE (varE xs) [m, failure])
        mkArg (fName, fDef) = do
          isP <- isPrim $ thFieldType fDef
          let x =
                if isP
                  then [|fromPrimitivePersistValue $(varE fName)|]
                  else varE fName
          maybe x (\convName -> [|snd $(varE convName) $ $x|]) $ thFieldConverter fDef
     in do
          xs <- newName "xs"
          let failureBody = normalB [|(\a -> error (failMessage a $(varE xs)) `asTypeOf` (a, [])) undefined|]
          failureName <- newName "failure"
          patVars <- mapM (const $ newName "x") fieldDefs
          let failure = match wildP (normalB $ varE failureName) []
              result = foldl (\a f -> appE a $ mkArg f) (conE cName) $ zip patVars fieldDefs
          (isFailureUsed, start) <- goPrim xs (zip patVars fieldDefs) result failure
          let failureFunc = funD failureName [clause [] failureBody []]
              locals = if isFailureUsed then [failureFunc] else []
          funD 'fromPurePersistValues [clause [varP xs] (normalB start) locals]

  let decs = [toPurePersistValues', fromPurePersistValues']
  pure [instanceD' context (AppT (ConT ''PurePersistField) dataType) decs]

mkEmbeddedPurePersistFieldInstance :: THEmbeddedDef -> Q [Dec]
mkEmbeddedPurePersistFieldInstance def = do
  let types = map extractType $ thEmbeddedTypeParams def
  let embedded = foldl AppT (ConT (thEmbeddedName def)) types
  let fDefs = thEmbeddedFields def
  context <- paramsPureContext (thEmbeddedTypeParams def) fDefs
  case context of
    Nothing -> pure []
    Just context' -> mkPurePersistFieldInstance embedded (thEmbeddedConstructorName def) fDefs context'

mkAutoKeyPersistFieldInstance :: THEntityDef -> Q [Dec]
mkAutoKeyPersistFieldInstance def = case thAutoKey def of
  Just _ -> do
    let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
    keyType <- [t|Key $(pure entity) BackendSpecific|]

    persistName' <- do
      a <- newName "a"
      let body = [|"Key" ++ [delim] ++ persistName ((undefined :: Key v u -> v) $(varE a))|]
      funD 'persistName [clause [varP a] (normalB body) []]
    toPersistValues' <- funD 'toPersistValues [clause [] (normalB [|primToPersistValue|]) []]
    fromPersistValues' <- funD 'fromPersistValues [clause [] (normalB [|primFromPersistValue|]) []]
    dbType' <- do
      proxy <- newName "p"
      a <- newName "a"
      let e = [|entityDef $(varE proxy) ((undefined :: Key v a -> v) $(varE a))|]
          body = [|DbTypePrimitive (getDefaultAutoKeyType $(varE proxy)) False Nothing (Just (Left ($e, Nothing), Nothing, Nothing))|]
      funD 'dbType [clause [varP proxy, varP a] (normalB body) []]

    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    let decs = [persistName', toPersistValues', fromPersistValues', dbType']
    pure [instanceD' context (AppT (ConT ''PersistField) keyType) decs]
  _ -> pure []

mkAutoKeyPrimitivePersistFieldInstance :: THEntityDef -> Q [Dec]
mkAutoKeyPrimitivePersistFieldInstance def = case thAutoKey def of
  Just autoKey -> do
    let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
    keyType <- [t|Key $(pure entity) BackendSpecific|]
    let conName = mkName $ thAutoKeyConstrName autoKey
    toPrim' <- do
      x <- newName "x"
      let body = [|$(varE x)|]
      funD 'toPrimitivePersistValue [clause [conP conName [varP x]] (normalB body) []]
    fromPrim' <- funD 'fromPrimitivePersistValue [clause [] (normalB $ conE conName) []]
    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    let decs = [toPrim', fromPrim']
    sequence
      [ pure $ instanceD' context (AppT (ConT ''PrimitivePersistField) keyType) decs,
        mkDefaultPurePersistFieldInstance context keyType,
        mkDefaultSinglePersistFieldInstance context keyType
      ]
  _ -> pure []

mkDefaultPurePersistFieldInstance :: Cxt -> Type -> Q Dec
mkDefaultPurePersistFieldInstance context typ = do
  toPurePersistValues' <- funD 'toPurePersistValues [clause [] (normalB [|primToPurePersistValues|]) []]
  fromPurePersistValues' <- funD 'fromPurePersistValues [clause [] (normalB [|primFromPurePersistValues|]) []]
  let decs = [toPurePersistValues', fromPurePersistValues']
  pure $ instanceD' context (AppT (ConT ''PurePersistField) typ) decs

mkDefaultSinglePersistFieldInstance :: Cxt -> Type -> Q Dec
mkDefaultSinglePersistFieldInstance context typ = do
  toSinglePersistValue' <- funD 'toSinglePersistValue [clause [] (normalB [|primToSinglePersistValue|]) []]
  fromSinglePersistValue' <- funD 'fromSinglePersistValue [clause [] (normalB [|primFromSinglePersistValue|]) []]
  let decs = [toSinglePersistValue', fromSinglePersistValue']
  pure $ instanceD' context (AppT (ConT ''SinglePersistField) typ) decs

mkUniqueKeysIsUniqueInstances :: THEntityDef -> Q [Dec]
mkUniqueKeysIsUniqueInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  let constr = head $ thConstructors def
  forM (thUniqueKeys def) $ \unique -> do
    uniqKeyType <- [t|Key $(pure entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique))|]
    extractUnique' <- do
      uniqueFields <- mapM (\f -> newName "x" >>= \x -> pure (thFieldName f, x)) $ thUniqueKeyFields unique
      let mkFieldPat f = maybe wildP varP $ lookup (thFieldName f) uniqueFields
      let pat = conP (thConstrName constr) $ map mkFieldPat $ thConstrFields constr
      let body = foldl (\expr f -> [|$expr $(varE $ snd f)|]) (conE $ mkName $ thUniqueKeyConstrName unique) uniqueFields
      funD 'extractUnique [clause [pat] (normalB body) []]
    uniqueNum' <- do
      let index = findIndex (\u -> thUniqueKeyName unique == thUniqueName u) $ thConstrUniques constr
      let uNum = fromMaybe (error $ "mkUniqueKeysIsUniqueInstances: cannot find unique definition for unique key " ++ thUniqueKeyName unique) index
      funD 'uniqueNum [clause [wildP] (normalB [|uNum|]) []]
    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    pure $ instanceD' context (AppT (ConT ''IsUniqueKey) uniqKeyType) [extractUnique', uniqueNum']

mkUniqueKeysEmbeddedInstances :: THEntityDef -> Q [Dec]
mkUniqueKeysEmbeddedInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  fmap concat $
    forM (filter thUniqueKeyMakeEmbedded $ thUniqueKeys def) $ \unique -> do
      uniqKeyType <- [t|Key $(pure entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique))|]
      let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
      mkEmbeddedInstance' uniqKeyType (thUniqueKeyFields unique) context

mkUniqueKeysPersistFieldInstances :: THEntityDef -> Q [Dec]
mkUniqueKeysPersistFieldInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  forM (thUniqueKeys def) $ \unique -> do
    uniqKeyType <- [t|Key $(pure entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique))|]

    persistName' <- funD 'persistName [clause [wildP] (normalB $ stringE $ thUniqueKeyDbName unique) []]

    toPersistValues' <- funD 'toPersistValues [clause [] (normalB [|pureToPersistValue|]) []]

    fromPersistValues' <- funD 'fromPersistValues [clause [] (normalB [|pureFromPersistValue|]) []]

    dbType' <- do
      a <- newName "a"
      proxy <- newName "p"
      let mkField f = do
            let fname = thDbFieldName f
                nvar = [|undefined :: $(pure $ thFieldType f)|]
                typ = mkType f proxy nvar
            [|(fname, $typ)|]
      let embedded = [|EmbeddedDef False $(listE $ map mkField $ thUniqueKeyFields unique)|]
          e = [|entityDef $(varE proxy) ((undefined :: Key v a -> v) $(varE a))|]
          body = [|DbEmbedded $embedded (Just (Left ($e, Just $(lift $ thUniqueKeyName unique)), Nothing, Nothing))|]
      funD 'dbType [clause [varP proxy, varP a] (normalB body) []]
    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    let decs = [persistName', toPersistValues', fromPersistValues', dbType']
    pure $ instanceD' context (AppT (ConT ''PersistField) uniqKeyType) decs

mkUniqueKeysPrimitiveOrPurePersistFieldInstances :: THEntityDef -> Q [Dec]
mkUniqueKeysPrimitiveOrPurePersistFieldInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  fmap concat $
    forM (thUniqueKeys def) $ \unique -> do
      uniqKeyType <- [t|Key $(pure entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique))|]
      let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
      let conName = mkName $ thUniqueKeyConstrName unique
      isUniquePrim <- case thUniqueKeyFields unique of
        [uniq] -> isPrim $ thFieldType uniq
        _ -> pure False
      if isUniquePrim
        then do
          x <- newName "x"
          toPrim' <- do
            funD 'toPrimitivePersistValue [clause [conP conName [varP x]] (normalB [|toPrimitivePersistValue $(varE x)|]) []]
          fromPrim' <- funD 'fromPrimitivePersistValue [clause [varP x] (normalB [|$(conE conName) (fromPrimitivePersistValue $(varE x))|]) []]
          let decs = [toPrim', fromPrim']
          sequence
            [ pure $ instanceD' context (AppT (ConT ''PrimitivePersistField) uniqKeyType) decs,
              mkDefaultPurePersistFieldInstance context uniqKeyType,
              mkDefaultSinglePersistFieldInstance context uniqKeyType
            ]
        else mkPurePersistFieldInstance uniqKeyType conName (thUniqueKeyFields unique) context

mkKeyEqShowInstances :: THEntityDef -> Q [Dec]
mkKeyEqShowInstances def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  let keysInfo =
        maybe [] (\k -> [(thAutoKeyConstrName k, 1, [t|BackendSpecific|])]) (thAutoKey def)
          ++ map (\k -> (thUniqueKeyConstrName k, length $ thUniqueKeyFields k, [t|Unique $(conT $ mkName $ thUniqueKeyPhantomName k)|])) (thUniqueKeys def)
  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  typ <- [t|Key $(pure entity) $(newName "a" >>= varT)|]

  showsPrec' <-
    let mkClause (cName, fieldsNum, _) = do
          p <- newName "p"
          fields <- replicateM fieldsNum (newName "x")
          let pat = conP (mkName cName) $ map varP fields
              showC = [|showString $(lift $ cName ++ " ")|]
              showArgs = foldr1 (\a b -> [|$a . showString " " . $b|]) $ map (\a -> [|showsPrec 11 $(varE a)|]) fields
              body = [|showParen ($(varE p) >= (11 :: Int)) ($showC . $showArgs)|]
          clause [varP p, pat] (normalB body) []
     in funD 'showsPrec $ map mkClause keysInfo

  eq' <-
    let mkClause (cName, fieldsNum, _) = do
          let fields = replicateM fieldsNum (newName "x")
          (fields1, fields2) <- liftM2 (,) fields fields
          let mkPat = conP (mkName cName) . map varP
              body = foldr1 (\e1 e2 -> [|$e1 && $e2|]) $ zipWith (\n1 n2 -> [|$(varE n1) == $(varE n2)|]) fields1 fields2
          clause [mkPat fields1, mkPat fields2] (normalB body) []
        clauses = map mkClause keysInfo
        noMatch = if length clauses > 1 then [clause [wildP, wildP] (normalB [|False|]) []] else []
     in funD '(==) $ clauses ++ noMatch

  read' <-
    let mkRead (cName, fieldsNum, u) = do
          let key =
                foldl (\a b -> [|$a <*> $b|]) [|$(conE $ mkName cName) <$> R.step R.readPrec|] $
                  replicate (fieldsNum - 1) [|R.step R.readPrec|]
              body = [|R.parens $ R.prec 10 $ R.expectP (R.Ident $(litE $ StringL cName)) >> $key|]
          keyType <- [t|Key $(pure entity) $u|]
          readPrec' <- funD 'R.readPrec [clause [] (normalB body) []]
          readListPrec' <- funD 'R.readListPrec [clause [] (normalB [|R.readListPrecDefault|]) []]
          pure $ instanceD' context (AppT (ConT ''Read) keyType) [readPrec', readListPrec']
     in mapM mkRead keysInfo

  pure $
    if null keysInfo
      then []
      else [instanceD' context (AppT (ConT ''Eq) typ) [eq'], instanceD' context (AppT (ConT ''Show) typ) [showsPrec']] ++ read'

mkEmbeddedInstance :: THEmbeddedDef -> Q [Dec]
mkEmbeddedInstance def = do
  let types = map extractType $ thEmbeddedTypeParams def
      embedded = foldl AppT (ConT (thEmbeddedName def)) types
      context = paramsContext (thEmbeddedTypeParams def) (thEmbeddedFields def)
  mkEmbeddedInstance' embedded (thEmbeddedFields def) context

mkEmbeddedInstance' :: Type -> [THFieldDef] -> Cxt -> Q [Dec]
mkEmbeddedInstance' dataType fDefs context = do
  selector' <- do
    fParam <- newName "f"
    let mkField field = ForallC [] [equalP' (VarT fParam) (thFieldType field)] $ NormalC (mkName $ thExprName field) []
    pure $ dataInstD' [] ''Selector [dataType, VarT fParam] (map mkField fDefs) []

  selectorNum' <- do
    let mkClause fNum field = clause [conP (mkName $ thExprName field) []] (normalB $ lift fNum) []
        clauses = zipWith mkClause [0 :: Int ..] fDefs
    funD 'selectorNum clauses

  let decs = [selector', selectorNum']
  pure [instanceD' context (AppT (ConT ''Embedded) dataType) decs]

mkEntityPhantomConstructors :: THEntityDef -> Q [Dec]
mkEntityPhantomConstructors def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  forM (thConstructors def) $ \c -> do
    v <- newName "v"
    let name = mkName $ thPhantomConstrName c
    phantom <- [t|ConstructorMarker $(pure entity)|]
    let constr = GadtC [name] [] (AppT (ConT name) phantom)
    pure $ dataD' [] name [plainTV v] [constr] []

mkEntityPhantomConstructorInstances :: THEntityDef -> Q [Dec]
mkEntityPhantomConstructorInstances def = zipWithM f [0 ..] (thConstructors def)
  where
    f :: Int -> THConstructorDef -> Q Dec
    f cNum c = instanceD (cxt []) (appT (conT ''Constructor) (conT $ mkName $ thPhantomConstrName c)) [phantomConstrNum']
      where
        phantomConstrNum' = funD 'phantomConstrNum [clause [wildP] (normalB [|cNum|]) []]

mkEntityUniqueKeysPhantoms :: THEntityDef -> Q [Dec]
mkEntityUniqueKeysPhantoms def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def
  fmap concat $
    forM (thUniqueKeys def) $ \u -> do
      exists <- lookupTypeName $ thUniqueKeyPhantomName u
      if isNothing exists
        then do
          v <- newName "v"
          let name = mkName $ thUniqueKeyPhantomName u
          phantom <- [t|UniqueMarker $(pure entity)|]
          let constr = GadtC [name] [] (AppT (ConT name) phantom)
          pure [dataD' [] name [plainTV v] [constr] []]
        else pure []

mkPersistEntityInstance :: THEntityDef -> Q [Dec]
mkPersistEntityInstance def = do
  let entity = foldl AppT (ConT (thDataName def)) $ map extractType $ thTypeParams def

  key' <- do
    uParam <- newName "u"
    autoKey <- case thAutoKey def of
      Nothing -> pure []
      Just k -> do
        keyDescr <- [t|BackendSpecific|]
        pure [ForallC [] [equalP' (VarT uParam) keyDescr] $ NormalC (mkName $ thAutoKeyConstrName k) [(notStrict', ConT ''PersistValue)]]
    uniques <- forM (thUniqueKeys def) $ \unique -> do
      uniqType <- [t|Unique $(conT $ mkName $ thUniqueKeyPhantomName unique)|]
      let cDef = head $ thConstructors def
          uniqFieldNames = lefts $ thUniqueFields $ findOne "unique" thUniqueName (thUniqueKeyName unique) $ thConstrUniques cDef
          uniqFields = concat $ flip map uniqFieldNames $ \name -> filter ((== name) . thFieldName) $ thConstrFields cDef
          uniqFields' = map (\f -> (notStrict', thFieldType f)) uniqFields
      pure $ ForallC [] [equalP' (VarT uParam) uniqType] $ NormalC (mkName $ thUniqueKeyConstrName unique) uniqFields'
    pure $ dataInstD' [] ''Key [entity, VarT uParam] (autoKey ++ uniques) []

  autoKey' <- do
    autoType <- case thAutoKey def of
      Nothing -> [t|()|]
      Just _ -> [t|Key $(pure entity) BackendSpecific|]
    pure $ mkTySynInstD ''AutoKey [entity] autoType

  defaultKey' <- do
    typ <- case thAutoKey def of
      Just k | thAutoKeyIsDef k -> [t|Key $(pure entity) BackendSpecific|]
      _ -> case filter thUniqueKeyIsDef $ thUniqueKeys def of
        [unique] -> [t|Key $(pure entity) (Unique $(conT $ mkName $ thUniqueKeyPhantomName unique))|]
        _ -> [t|()|]
    pure $ mkTySynInstD ''DefaultKey [entity] typ

  isSumType' <- do
    let isSumType =
          ConT $
            if length (thConstructors def) == 1
              then ''HFalse
              else ''HTrue
    pure $ mkTySynInstD ''IsSumType [entity] isSumType

  fields' <- do
    cParam <- newName "c"
    fParam <- newName "f"
    let mkField name field = ForallC [] [equalP' (VarT cParam) (ConT name), equalP' (VarT fParam) (thFieldType field)] $ NormalC (mkName $ thExprName field) []
    let f cdef = map (mkField $ mkName $ thPhantomConstrName cdef) $ thConstrFields cdef
    let constrs = concatMap f $ thConstructors def
    pure $ dataInstD' [] ''Field [entity, VarT cParam, VarT fParam] constrs []

  entityDef' <- do
    v <- newName "v"
    proxy <- newName "p"
    let mkLambda t = [|undefined :: $(pure entity) -> $(pure t) |]
        types = map extractType $ thTypeParams def
        typeParams' = listE $ map (\t -> [|dbType $(varE proxy) ($(mkLambda t) $(varE v))|]) types
        mkField c fNum f = do
          a <- newName "a"
          let fname = thDbFieldName f
              nvar =
                if hasFreeVars (thFieldType f)
                  then
                    let pat = conP (thConstrName c) $ replicate fNum wildP ++ [varP a] ++ replicate (length (thConstrFields c) - fNum - 1) wildP
                        wildClause = if length (thConstructors def) > 1 then [match wildP (normalB [|undefined|]) []] else []
                     in caseE (varE v) (match pat (normalB $ varE a) [] : wildClause)
                  else [|undefined :: $(pure $ thFieldType f)|]
              typ = mkType f proxy nvar
          [|(fname, $typ)|]
        constrs = listE $ map mkConstructorDef $ thConstructors def
        mkConstructorDef c@(THConstructorDef _ _ name keyName params conss) = [|ConstructorDef name keyName $(listE $ map snd fields) $(listE $ map mkConstraint conss)|]
          where
            fields = zipWith (\i f -> (thFieldName f, mkField c i f)) [0 ..] params
            mkConstraint (THUniqueDef uName uType uFields) = [|UniqueDef (Just uName) uType $(listE $ map getField uFields)|]
            getField (Left fName) = [|Left $(snd $ findOne "field" fst fName fields)|]
            getField (Right expr) = [|Right expr|]

        paramNames = foldr1 (\p xs -> [|$p ++ [delim] ++ $xs|]) $ map (\t -> [|persistName ($(mkLambda t) $(varE v))|]) types
        fullEntityName =
          if null types
            then [|$(stringE $ thDbEntityName def)|]
            else [|$(stringE $ thDbEntityName def) ++ [delim] ++ $(paramNames)|]

        body = normalB [|EntityDef $fullEntityName $(lift $ thEntitySchema def) $typeParams' $constrs|]
        entityPat = if null $ thTypeParams def then wildP else varP v
    funD 'entityDef [clause [varP proxy, entityPat] body []]

  toEntityPersistValues' <- fmap (FunD 'toEntityPersistValues) $
    forM (zip [0 :: Int ..] $ thConstructors def) $ \(cNum, c) -> do
      (pat, body) <- mkToPersistValues (thConstrName c) (thConstrFields c) (\result -> [|(toPrimitivePersistValue ($(lift cNum) :: Int) :) . $result|])
      clause [pat] (normalB body) []

  fromEntityPersistValues' <- do
    xs <- newName "xs"
    let failureBody = normalB [|fail (failMessageNamed $(stringE $ show $ thDataName def) $(varE xs))|]
    failureName <- newName "failure"
    let failure = match wildP (normalB $ varE failureName) []
    matches <- forM (zip [0 ..] (thConstructors def)) $ \(cNum, c) -> do
      let cNum' = conP 'PersistInt64 [litP $ integerL cNum]
      xs' <- newName "xs"
      (_, body) <- mkFromPersistValues failureName xs' (thConstrName c) (thConstrFields c)
      pure $ match (infixP cNum' '(:) (varP xs')) (normalB $ pure body) []
    let start = caseE (varE xs) $ matches ++ [failure]
    let failureFunc = funD failureName [clause [] failureBody []]
    funD 'fromEntityPersistValues [clause [varP xs] (normalB start) [failureFunc]]

  getUniques' <-
    let hasConstraints = not . null . thConstrUniques
        clauses = zipWith mkClause [0 :: Int ..] (thConstructors def)
        mkClause cNum cdef | not (hasConstraints cdef) = clause [conP (thConstrName cdef) pats] (normalB [|(cNum, [])|]) []
          where
            pats = map (const wildP) $ thConstrFields cdef
        mkClause cNum cdef = do
          let allConstrainedFields = lefts $ concatMap thUniqueFields $ thConstrUniques cdef
          vars <- mapM (\f -> newName "x" >>= \x -> pure $ if thFieldName f `elem` allConstrainedFields then Just (x, f) else Nothing) $ thConstrFields cdef
          let pat = conP (thConstrName cdef) $ map (maybe wildP (varP . fst)) vars
              body = normalB [|(cNum, $(listE $ mapMaybe mkUnique $ thConstrUniques cdef))|]
              mkUnique (THUniqueDef uName _ fnames) =
                if null $ rights fnames
                  then
                    let -- find corresponding field from vars
                        uFields = map (\f -> findOne "field" (thFieldName . snd) f $ catMaybes vars) $ lefts fnames
                        result = mkToPurePersistValues uFields
                     in Just [|(uName, $result)|]
                  else Nothing
          clause [pat] body []
     in funD 'getUniques clauses

  entityFieldChain' <-
    let thFieldNames = thConstructors def >>= thConstrFields
        clauses = map mkClause thFieldNames
        mkClause f = do
          fArg <- newName "f"
          proxy <- newName "p"
          let nvar = [|(undefined :: Field v c a -> a) $(varE fArg)|]
              typ = mkType f proxy nvar
              body = [|(($(lift $ thDbFieldName f), $typ), [])|]
          clause [varP proxy, asP fArg $ conP (mkName $ thExprName f) []] (normalB body) []
        clauses' = if null clauses then [clause [wildP] (normalB [|undefined|]) []] else clauses
     in funD 'entityFieldChain clauses'

  let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  let decs = [key', autoKey', defaultKey', isSumType', fields', entityDef', toEntityPersistValues', fromEntityPersistValues', getUniques', entityFieldChain']
  pure [instanceD' context (AppT (ConT ''PersistEntity) entity) decs]

mkToPurePersistValues :: [(Name, THFieldDef)] -> Q Exp
mkToPurePersistValues vars = do
  let processField (fName, fDef) = do
        isP <- isPrim (thFieldType fDef)
        let field = maybe id (\convName x -> [|fst $(varE convName) $ $x|]) (thFieldConverter fDef) (varE fName)
        if isP
          then pure (Nothing, [|(toPrimitivePersistValue $field :)|])
          else newName "x" >>= \x -> pure (Just $ valD (varP x) (normalB [|toPurePersistValues $(varE fName)|]) [], varE x)
  (lets, funcs) <- fmap (first catMaybes . unzip) $ mapM processField vars
  let result = if null funcs then [|id|] else foldr1 (\a b -> [|$a . $b|]) funcs
  if null lets then result else letE lets result

mkEntityPersistFieldInstance :: THEntityDef -> Q [Dec]
mkEntityPersistFieldInstance def = case getDefaultKey def of
  Just defaultKey -> do
    let types = map extractType $ thTypeParams def
    let entity = foldl AppT (ConT (thDataName def)) types

    persistName' <- do
      v <- newName "v"
      let mkLambda t = [|undefined :: $(pure entity) -> $(pure t) |]

      let paramNames = foldr1 (\p xs -> [|$p ++ [delim] ++ $xs|]) $ map (\t -> [|persistName ($(mkLambda t) $(varE v))|]) types
      let fullEntityName = case types of
            [] -> [|$(stringE $ thDbEntityName def)|]
            _ -> [|$(stringE $ thDbEntityName def) ++ [delim] ++ $(paramNames)|]
      let body = normalB fullEntityName
      let pat = if null types then wildP else varP v
      funD 'persistName [clause [pat] body []]

    isOne <- isDefaultKeyOneColumn def
    let mUniqName = either auto uniq defaultKey
          where
            auto _ = Nothing
            uniq u = Just $ mkName $ thUniqueKeyPhantomName u

    toPersistValues' <- do
      let body = normalB $ case mUniqName of
            _ | isOne -> [|singleToPersistValue|]
            Just name -> [|toPersistValuesUnique $(conE name)|]
            _ -> error "mkEntityPersistFieldInstance: key has no unique type"
      funD 'toPersistValues [clause [] body []]

    fromPersistValues' <- do
      let body = normalB $ case mUniqName of
            _ | isOne -> [|singleFromPersistValue|]
            Just name -> [|fromPersistValuesUnique $(conE name)|]
            _ -> error "mkEntityPersistFieldInstance: key has no unique type"
      funD 'fromPersistValues [clause [] body []]

    dbType' <- do
      proxy <- newName "p"
      let body = [|dbType $(varE proxy) . (undefined :: a -> DefaultKey a)|]
      funD 'dbType [clause [varP proxy] (normalB body) []]

    let context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
    let decs = [persistName', toPersistValues', fromPersistValues', dbType']
    pure [instanceD' context (AppT (ConT ''PersistField) entity) decs]
  Nothing -> pure []

mkEntitySinglePersistFieldInstance :: THEntityDef -> Q [Dec]
mkEntitySinglePersistFieldInstance def =
  isDefaultKeyOneColumn def >>= \isOne -> case getDefaultKey def of
    Just defaultKey | isOne -> do
      let types = map extractType $ thTypeParams def
          entity = foldl AppT (ConT (thDataName def)) types
          context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)

          (to, from) = case defaultKey of
            Left _ -> ([|toSinglePersistValueAutoKey|], [|fromSinglePersistValueAutoKey|])
            Right k -> ([|toSinglePersistValueUnique $u|], [|fromSinglePersistValueUnique $u|])
              where
                u = conE $ mkName $ thUniqueKeyPhantomName k

      toSinglePersistValue' <- funD 'toSinglePersistValue [clause [] (normalB to) []]
      fromSinglePersistValue' <- funD 'fromSinglePersistValue [clause [] (normalB from) []]
      let decs = [toSinglePersistValue', fromSinglePersistValue']
      pure [instanceD' context (AppT (ConT ''SinglePersistField) entity) decs]
    _ -> pure []

mkEntityNeverNullInstance :: THEntityDef -> Q [Dec]
mkEntityNeverNullInstance def = do
  let types = map extractType $ thTypeParams def
      entity = foldl AppT (ConT (thDataName def)) types
      context = paramsContext (thTypeParams def) (thConstructors def >>= thConstrFields)
  isOne <- isDefaultKeyOneColumn def
  pure $
    if isOne
      then [instanceD' context (AppT (ConT ''NeverNull) entity) []]
      else []

mkPrimitivePersistFieldInstance :: THPrimitiveDef -> Q [Dec]
mkPrimitivePersistFieldInstance def = do
  let primitive = ConT (thPrimitiveName def)
  persistName' <- do
    let body = normalB $ stringE $ nameBase $ thPrimitiveName def
    funD 'persistName [clause [wildP] body []]
  fromPersistValues' <- funD 'fromPersistValues [clause [] (normalB [|primFromPersistValue|]) []]
  toPersistValues' <- funD 'toPersistValues [clause [] (normalB [|primToPersistValue|]) []]
  dbType' <- do
    proxy <- newName "p"
    x <- newName "x"
    let body = [|dbType $(varE proxy) $ fst $(varE $ thPrimitiveConverter def) $(varE x)|]
    funD 'dbType [clause [varP proxy, varP x] (normalB body) []]
  let decs = [persistName', toPersistValues', fromPersistValues', dbType']
  pure
    [ instanceD' [] (AppT (ConT ''PersistField) primitive) decs,
      instanceD' [] (AppT (ConT ''NeverNull) primitive) []
    ]

mkPrimitivePrimitivePersistFieldInstance :: THPrimitiveDef -> Q [Dec]
mkPrimitivePrimitivePersistFieldInstance def = do
  let primitive = ConT (thPrimitiveName def)
  toPrim' <- do
    let body = [|toPrimitivePersistValue . fst $(varE $ thPrimitiveConverter def)|]
    funD 'toPrimitivePersistValue [clause [] (normalB body) []]
  fromPrim' <- do
    let body = [|snd $(varE $ thPrimitiveConverter def) . fromPrimitivePersistValue|]
    funD 'fromPrimitivePersistValue [clause [] (normalB body) []]
  let context = []
  let decs = [toPrim', fromPrim']
  sequence
    [ pure $ instanceD' context (AppT (ConT ''PrimitivePersistField) primitive) decs,
      mkDefaultPurePersistFieldInstance context primitive,
      mkDefaultSinglePersistFieldInstance context primitive
    ]

mkMigrateFunction :: String -> [THEntityDef] -> Q [Dec]
mkMigrateFunction name defs = do
  let (normal, polymorhpic) = partition (null . thTypeParams) defs
  forM_ polymorhpic $ \def -> reportWarning $ "Datatype " ++ show (thDataName def) ++ " will not be migrated automatically by function " ++ name ++ " because it has type parameters"
  let body = doE $ map (\def -> noBindS [|migrate (undefined :: $(conT $ thDataName def))|]) normal
  sig <- sigD (mkName name) [t|forall m. PersistBackend m => Migration m|]
  func <- funD (mkName name) [clause [] (normalB body) []]
  pure [sig, func]

isDefaultKeyOneColumn :: THEntityDef -> Q Bool
isDefaultKeyOneColumn def = case getDefaultKey def of
  Just (Left _) -> pure True
  Just (Right unique) -> case thUniqueKeyFields unique of
    [field] -> isPrim $ thFieldType field
    _ -> pure False
  _ -> pure False

getDefaultKey :: THEntityDef -> Maybe (Either THAutoKeyDef THUniqueKeyDef)
getDefaultKey def = case thAutoKey def of
  Just k | thAutoKeyIsDef k -> Just $ Left k
  _ -> case filter thUniqueKeyIsDef $ thUniqueKeys def of
    [] -> Nothing
    (u : _) -> Just $ Right u

#if MIN_VERSION_template_haskell(2, 17, 0)
paramsContext :: [TH.TyVarBndr flag] -> [THFieldDef] -> Cxt
#else
paramsContext :: [TyVarBndr] -> [THFieldDef] -> Cxt
#endif
paramsContext types fields = classPred ''PersistField params ++ classPred ''SinglePersistField maybys ++ classPred ''NeverNull maybys
  where
    classPred clazz = map (\t -> classP' clazz [t])
    -- every type must be an instance of PersistField
    params = map extractType types
    -- all datatype fields also must be instances of PersistField
    -- if Maybe is applied to a type param, the param must be also an instance of NeverNull
    -- so that (Maybe param) is an instance of PersistField
    maybys = nub $ fields >>= insideMaybe . thFieldType

#if MIN_VERSION_template_haskell(2, 17, 0)
paramsPureContext :: [TH.TyVarBndr flag] -> [THFieldDef] -> Q (Maybe Cxt)
#else
paramsPureContext :: [TyVarBndr] -> [THFieldDef] -> Q (Maybe Cxt)
#endif
paramsPureContext types fields = do
  let isValidType (VarT _) = pure True
      isValidType t = isPrim t
  invalid <- filterM (fmap not . isValidType . thFieldType) fields
  pure $ case invalid of
    [] -> Just $ classPred ''PurePersistField params ++ classPred ''PrimitivePersistField maybys ++ classPred ''NeverNull maybys
      where
        params = map extractType types
        classPred clazz = map (\t -> classP' clazz [t])
        -- all datatype fields also must be instances of PersistField
        -- if Maybe is applied to a type param, the param must be also an instance of NeverNull
        -- so that (Maybe param) is an instance of PersistField
        maybys = nub $ fields >>= insideMaybe . thFieldType
    _ -> Nothing

#if MIN_VERSION_template_haskell(2, 17, 0)
extractType :: TH.TyVarBndr flag -> Type
extractType (PlainTV name _) = VarT name
extractType (KindedTV name _ _) = VarT name
#else
extractType :: TyVarBndr -> Type
extractType (PlainTV name) = VarT name
extractType (KindedTV name _) = VarT name
#endif

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
isPrim t | hasFreeVars t = pure False
isPrim t@(ConT _) = isClassInstance ''PrimitivePersistField [t]
--isPrim (AppT (ConT key) _)  | key == ''Key = return True
isPrim (AppT (AppT (ConT key) _) (AppT (AppT _ (ConT typ)) _)) | key == ''Key && typ == ''BackendSpecific = pure True
isPrim (AppT (ConT tcon) t) | tcon == ''Maybe = isPrim t
isPrim _ = pure False

foldType :: (Type -> a) -> (a -> a -> a) -> Type -> a
foldType f app = go
  where
    go ForallT {} = error "forall'ed fields are not allowed"
    go z@(AppT a b) = f z `app` go a `app` go b
    go z@(SigT t _) = f z `app` go t
    go z = f z

hasFreeVars :: Type -> Bool
hasFreeVars = foldType f (||)
  where
    f (VarT _) = True
    f _ = False

insideMaybe :: Type -> [Type]
insideMaybe = foldType f (++)
  where
    f (AppT (ConT c) t@(VarT _)) | c == ''Maybe = [t]
    f _ = []

spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM p = go
  where
    go [] = pure ([], [])
    go (x : xs) = do
      flg <- p x
      if flg
        then do
          (ys, zs) <- go xs
          pure (x : ys, zs)
        else pure ([], x : xs)

mkType :: THFieldDef -> Name -> ExpQ -> ExpQ
mkType THFieldDef {..} proxy nvar = t3
  where
    psField = PSFieldDef thFieldName (Just thDbFieldName) thDbTypeName (Just thExprName) thEmbeddedDef thDefaultValue thReferenceParent (fmap show thFieldConverter)
    t1 = maybe id (\convName x -> [|fst $(varE convName) $ $x|]) thFieldConverter nvar
    t2 = [|dbType $(varE proxy) $t1|]
    -- if there are any type settings, apply them in runtime
    t3 = case (thDbTypeName, thEmbeddedDef, thDefaultValue, thReferenceParent) of
      (Nothing, Nothing, Nothing, Nothing) -> t2
      _ -> [|applyDbTypeSettings $(lift psField) $t2|]

mkTySynInstD :: Name -> [Type] -> Type -> Dec
#if MIN_VERSION_template_haskell(2, 15, 0)
mkTySynInstD name ts t =
  TySynInstD $ TySynEqn Nothing typ t where
    typ = foldl AppT (ConT name) ts
#elif MIN_VERSION_template_haskell(2, 9, 0)
mkTySynInstD name ts t = TySynInstD name $ TySynEqn ts t
#else
mkTySynInstD = TySynInstD
#endif

classP' :: Name -> [Type] -> Pred
#if MIN_VERSION_template_haskell(2, 10, 0)
classP' name ts = foldl AppT (ConT name) ts
#else
classP' = ClassP
#endif

equalP' :: Type -> Type -> Pred
#if MIN_VERSION_template_haskell(2, 10, 0)
equalP' t1 t2 = foldl AppT EqualityT [t1, t2]
#else
equalP'= EqualP
#endif

instanceD' :: Cxt -> Type -> [Dec] -> InstanceDec
#if MIN_VERSION_template_haskell(2, 11, 0)
instanceD' = InstanceD Nothing
#else
instanceD' = InstanceD
#endif

dataInstD' :: Cxt -> Name -> [Type] -> [Con] -> [Name] -> InstanceDec
#if MIN_VERSION_template_haskell(2, 15, 0)
dataInstD' context name types constrs derives =
  DataInstD context Nothing typ Nothing constrs [DerivClause Nothing (map ConT derives)] where
    typ = foldl AppT (ConT name) types
#elif MIN_VERSION_template_haskell(2, 12, 0)
dataInstD' context name types constrs derives =
  DataInstD context name types Nothing constrs [DerivClause Nothing (map ConT derives)]
#elif MIN_VERSION_template_haskell(2, 11, 0)
dataInstD' context name types constrs derives =
  DataInstD context name types Nothing constrs (map ConT derives)
#else
dataInstD' = DataInstD
#endif

dataD' :: Cxt -> Name -> [TyVarBndr] -> [Con] -> [Name] -> InstanceDec
#if MIN_VERSION_template_haskell(2, 12, 0)
dataD' context name types constrs derives =
  DataD context name types Nothing constrs [DerivClause Nothing (map ConT derives)]
#elif MIN_VERSION_template_haskell(2, 11, 0)
dataD' context name types constrs derives =
  DataD context name types Nothing constrs (map ConT derives)
#else
dataD' = DataD
#endif

#if MIN_VERSION_template_haskell(2, 11, 0)
notStrict' :: Bang
notStrict' = Bang NoSourceUnpackedness NoSourceStrictness
#else
notStrict' :: Strict
notStrict' = NotStrict
#endif

type TyVarBndr =
#if MIN_VERSION_template_haskell(2, 17, 0)
  TH.TyVarBndr ()
#else
  TH.TyVarBndr
#endif
