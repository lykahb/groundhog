{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module defines the functions which are used only for backends creation.
module Database.Groundhog.Generic.Sql
    ( 
    -- * SQL rendering utilities
      renderCond
    , defaultShowPrim
    , renderOrders
    , renderUpdates
    , renderFields
    , renderChain
    , renderExpr
    , renderExprPriority
    , renderExprExtended
    , renderPersistValue
    , intercalateS
    , commasJoin
    , RenderS(..)
    , Utf8(..)
    , fromUtf8
    , StringLike(..)
    , fromString
    , (<>)
    , function
    , operator
    , parens
    , Snippet(..)
    , SqlDb
    , liftExpr
    ) where

import Database.Groundhog.Core
import Database.Groundhog.Instances ()
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.String

import Database.Groundhog.Expression

class (Monoid a, IsString a) => StringLike a where
  fromChar :: Char -> a

data RenderS db r = RenderS {
    getQuery  :: Utf8
  , getValues :: [PersistValue] -> [PersistValue]
}

instance Monoid Utf8 where
  mempty = Utf8 mempty
  mappend (Utf8 a) (Utf8 b) = Utf8 (mappend a b)

instance IsString Utf8 where
  fromString = Utf8 . B.fromString

instance StringLike Utf8 where
  fromChar = Utf8 . B.fromChar

-- | Escape function, priority of the outer operator. The result is a list for the embedded data which may expand to several RenderS.
newtype Snippet db r = Snippet ((Utf8 -> Utf8) -> Int -> [RenderS db r])

-- Alas, GHC before 7.2 does not support superclass equality constraints (QueryRaw db ~ Snippet db). 
class DbDescriptor db => SqlDb db

renderExpr :: (DbDescriptor db, QueryRaw db ~ Snippet db) => (Utf8 -> Utf8) -> UntypedExpr db r -> RenderS db r
renderExpr esc expr = renderExprPriority esc 0 expr

renderExprPriority :: (DbDescriptor db, QueryRaw db ~ Snippet db) => (Utf8 -> Utf8) -> Int -> UntypedExpr db r -> RenderS db r
renderExprPriority esc p expr = (case expr of
  ExprRaw (Expr (Snippet f)) -> let vals = f esc p in ensureOne vals id
  ExprField f -> let fs = renderChain esc f []
                 in ensureOne fs $ \f' -> RenderS f' id
  ExprPure  a -> let vals = toPurePersistValues proxy a
                 in ensureOne (vals []) renderPersistValue) where
    proxy = (undefined :: f db r -> Proxy db) expr
    ensureOne :: [a] -> (a -> b) -> b
    ensureOne xs f = case xs of
      [x] -> f x
      xs' -> error $ "renderExprPriority: expected one column field, found " ++ show (length xs')

renderExprExtended :: (DbDescriptor db, QueryRaw db ~ Snippet db) => (Utf8 -> Utf8) -> Int -> UntypedExpr db r -> [RenderS db r]
renderExprExtended esc p expr = (case expr of
  ExprRaw (Expr (Snippet f)) -> f esc p
  ExprField f -> map (flip RenderS id) $ renderChain esc f []
  ExprPure a -> let vals = toPurePersistValues proxy a []
                in map renderPersistValue vals) where
  proxy = (undefined :: f db r -> Proxy db) expr

renderPersistValue :: PersistValue -> RenderS db r
renderPersistValue (PersistCustom s as) = RenderS s (as++)
renderPersistValue a = RenderS (fromChar '?') (a:)

instance Monoid (RenderS db r) where
  mempty = RenderS mempty id
  (RenderS f1 g1) `mappend` (RenderS f2 g2) = RenderS (f1 `mappend` f2) (g1 . g2)

instance IsString (RenderS db r) where
  fromString s = RenderS (fromString s) id

instance StringLike (RenderS db r) where
  fromChar c = RenderS (fromChar c) id

-- Has bad performance. This instance exists only for testing purposes
instance StringLike String where
  fromChar c = [c]

{-# INLINABLE parens #-}
parens :: Int -> Int -> RenderS db r -> RenderS db r
parens p1 p2 expr = if p1 < p2 then fromChar '(' <> expr <> fromChar ')' else expr

operator :: (SqlDb db, QueryRaw db ~ Snippet db, Expression db r a, Expression db r b) => Int -> String -> a -> b -> Snippet db r
operator pr op = \a b -> Snippet $ \esc p ->
  [parens pr p $ renderExprPriority esc pr (toExpr a) <> fromString op <> renderExprPriority esc pr (toExpr b)]

function :: (SqlDb db, QueryRaw db ~ Snippet db) => String -> [UntypedExpr db r] -> Snippet db r
function func args = Snippet $ \esc _ -> [fromString func <> fromChar '(' <> commasJoin (map (renderExpr esc) args) <> fromChar ')']

#if !MIN_VERSION_base(4, 5, 0)
{-# INLINABLE (<>) #-}
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

instance (SqlDb db, QueryRaw db ~ Snippet db, PersistField a, Num a) => Num (Expr db r a) where
  a + b = Expr $ operator 60 "+" a b
  a - b = Expr $ operator 60 "-" a b
  a * b = Expr $ operator 70 "*" a b
  signum = error "Num Expr: no signum"
  abs a = Expr $ Snippet $ \esc _ -> ["ABS(" <> renderExpr esc (toExpr a) <> fromChar ')']
  fromInteger a = liftExpr' (fromIntegral a :: Int64)

{-# INLINABLE renderCond #-}
-- | Renders conditions for SQL backend. Returns Nothing if the fields don't have any columns.
renderCond :: forall r db . (SqlDb db, QueryRaw db ~ Snippet db)
  => (Utf8 -> Utf8) -- ^ escape
  -> (Utf8 -> Utf8 -> Utf8) -- ^ render equals
  -> (Utf8 -> Utf8 -> Utf8) -- ^ render not equals
  -> Cond db r -> Maybe (RenderS db r)
renderCond esc rendEq rendNotEq (cond :: Cond db r) = go cond 0 where
  go (And a b)       p = perhaps andP p " AND " a b
  go (Or a b)        p = perhaps orP p " OR " a b
  go (Not a)         p = fmap (\a' -> parens notP p $ "NOT " <> a') $ go a notP
  go (Compare op f1 f2) p = case op of
    Eq -> renderComp andP p " AND " rendEq f1 f2
    Ne -> renderComp orP p " OR " rendNotEq f1 f2
    Gt -> renderComp orP p " OR " (\a b -> a <> fromChar '>' <> b) f1 f2
    Lt -> renderComp orP p " OR " (\a b -> a <> fromChar '<' <> b) f1 f2
    Ge -> renderComp orP p " OR " (\a b -> a <> ">=" <> b) f1 f2
    Le -> renderComp orP p " OR " (\a b -> a <> "<=" <> b) f1 f2
  go (CondRaw (Snippet f)) p = case f esc p of
    [] -> Nothing
    [a] -> Just a
    _ -> error "renderCond: cannot render CondRaw with many elements"
  notP = 35
  andP = 30
  orP = 20

  renderComp p pOuter logicOp op expr1 expr2 = result where
    expr1' = renderExprExtended esc p' expr1
    expr2' = renderExprExtended esc p' expr2
    liftOp f (RenderS a1 b1) (RenderS a2 b2) = RenderS (f a1 a2) (b1 . b2)
    (result, p') = case zipWith (liftOp op) expr1' expr2' of
      [clause] -> (Just clause, pOuter)
      [] -> (Nothing, pOuter)
      clauses -> (Just $ parens p pOuter $ intercalateS logicOp clauses, p)
  perhaps :: Int -> Int -> Utf8 -> Cond db r -> Cond db r -> Maybe (RenderS db r)
  perhaps p pOuter op a b = result where
    -- we don't know if the current operator is present until we render both operands. Rendering requires priority of the outer operator. We tie a knot to defer calculating the priority
    (priority, result) = case (go a priority, go b priority) of
       (Just a', Just b') -> (p, Just $ parens p pOuter $ a' <> RenderS op id <> b')
       (Just a', Nothing) -> (pOuter, Just a')
       (Nothing, Just b') -> (pOuter, Just b')
       (Nothing, Nothing) -> (pOuter, Nothing)

{-
examples of prefixes
[("val1", DbEmbedded False _), ("val4", EmbeddedDef False _), ("val5", EmbeddedDef False _)] -> "val5$val4$val1"
[("val1", DbEmbedded True _),  ("val4", EmbeddedDef False _), ("val5", EmbeddedDef False _)] -> ""
[("val1", DbEmbedded False _), ("val4", EmbeddedDef True _),  ("val5", EmbeddedDef False _)] -> "val1"
[("val1", DbEmbedded False _), ("val4", EmbeddedDef True _),  ("val5", EmbeddedDef True _)] -> "val1"
[("val1", DbEmbedded False _), ("val4", EmbeddedDef False _), ("val5", EmbeddedDef True _)] -> "val4$val1"
-}
{-# INLINABLE renderChain #-}
renderChain :: (Utf8 -> Utf8) -> FieldChain -> [Utf8] -> [Utf8]
renderChain esc (f, prefix) acc = (case prefix of
  ((name, EmbeddedDef False _):fs) -> flattenP esc (goP (fromString name) fs) f acc
  _ -> flatten esc f acc) where
  goP p ((name, EmbeddedDef False _):fs) = goP (fromString name <> fromChar delim <> p) fs
  goP p _ = p

defaultShowPrim :: PersistValue -> String
defaultShowPrim (PersistString x) = "'" ++ x ++ "'"
defaultShowPrim (PersistByteString x) = "'" ++ show x ++ "'"
defaultShowPrim (PersistInt64 x) = show x
defaultShowPrim (PersistDouble x) = show x
defaultShowPrim (PersistBool x) = if x then "1" else "0"
defaultShowPrim (PersistDay x) = show x
defaultShowPrim (PersistTimeOfDay x) = show x
defaultShowPrim (PersistUTCTime x) = show x
defaultShowPrim (PersistZonedTime x) = show x
defaultShowPrim (PersistNull) = "NULL"
defaultShowPrim (PersistCustom _ _) = error "Unexpected PersistCustom"

{-# INLINABLE renderOrders #-}
renderOrders :: forall db r . (Utf8 -> Utf8) -> [Order db r] -> Utf8
renderOrders _ [] = mempty
renderOrders esc xs = if null orders then mempty else " ORDER BY " <> commasJoin orders where
  orders = foldr go [] xs
  go (Asc a) acc = renderChain esc (fieldChain a) acc
  go (Desc a) acc = renderChain (\f -> esc f <> " DESC") (fieldChain a) acc

{-# INLINABLE renderFields #-}
-- Returns string with comma separated escaped fields like "name,age"
-- If there are other columns before renderFields result, do not put comma because the result might be an empty string. This happens when the fields have no columns like ().
-- One of the solutions is to add one more field with datatype that is known to have columns, eg renderFields id (("id", namedType (0 :: Int64)) : constrParams constr)
renderFields :: (Utf8 -> Utf8) -> [(String, DbType)] -> Utf8
renderFields esc = commasJoin . foldr (flatten esc) []

-- TODO: merge code of flatten and flattenP
flatten :: (Utf8 -> Utf8) -> (String, DbType) -> ([Utf8] -> [Utf8])
flatten esc (fname, typ) acc = go typ where
  go typ' = case typ' of
    DbMaybe t -> go t
    DbEmbedded emb -> handleEmb emb
    DbEntity (Just (emb, _)) _ -> handleEmb emb
    _            -> esc fullName : acc
  fullName = fromString fname
  handleEmb (EmbeddedDef False ts) = foldr (flattenP esc fullName) acc ts
  handleEmb (EmbeddedDef True  ts) = foldr (flatten esc) acc ts

flattenP :: (Utf8 -> Utf8) -> Utf8 -> (String, DbType) -> ([Utf8] -> [Utf8])
flattenP esc prefix (fname, typ) acc = go typ where
  go typ' = case typ' of
    DbMaybe t -> go t
    DbEmbedded emb -> handleEmb emb
    DbEntity (Just (emb, _)) _ -> handleEmb emb
    _            -> esc fullName : acc
  fullName = prefix <> fromChar delim <> fromString fname
  handleEmb (EmbeddedDef False ts) = foldr (flattenP esc fullName) acc ts
  handleEmb (EmbeddedDef True  ts) = foldr (flatten esc) acc ts

commasJoin :: StringLike s => [s] -> s
commasJoin = intercalateS (fromChar ',')

{-# INLINEABLE intercalateS #-}
intercalateS :: StringLike s => s -> [s] -> s
intercalateS _ [] = mempty
intercalateS a (x:xs) = x <> go xs where
  go [] = mempty
  go (f:fs) = a <> f <> go fs

{-# INLINABLE renderUpdates #-}
renderUpdates :: (SqlDb db, QueryRaw db ~ Snippet db) => (Utf8 -> Utf8) -> [Update db r] -> Maybe (RenderS db r)
renderUpdates esc upds = (case mapMaybe go upds of
  [] -> Nothing
  xs -> Just $ commasJoin xs) where
  go (Update field expr) = guard $ commasJoin $ zipWith (\f1 f2 -> f1 <> fromChar '=' <> f2) fs (rend expr) where
    rend = renderExprExtended esc 0
    fs = concatMap rend (projectionExprs field [])
    guard a = if null fs then Nothing else Just a

liftExpr :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r a b) => a -> Expr db r b
liftExpr a = liftExpr' a

liftExpr' :: (SqlDb db, QueryRaw db ~ Snippet db, Expression db r a) => a -> Expr db r b
liftExpr' a = Expr $ Snippet $ \esc pr -> renderExprExtended esc pr (toExpr a)
