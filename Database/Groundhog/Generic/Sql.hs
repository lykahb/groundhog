{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE CPP #-}

-- | This module defines the functions which are used only for backends creation.
module Database.Groundhog.Generic.Sql
    ( renderCond
    , defaultShowPrim
    , renderArith
    , renderOrders
    , renderUpdates
    , defDelim
    , renderFields
    , renderChain
    , intercalateS
    , RenderS(..)
    , StringLike(..)
    , fromString
    , (<>)
    , parens
    ) where

import Database.Groundhog.Core
import Database.Groundhog.Instances ()
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.String

class (Monoid a, IsString a) => StringLike a where
  fromChar :: Char -> a

data RenderS s = RenderS {
    getQuery  :: s
  , getValues :: [PersistValue] -> [PersistValue]
}

instance Monoid s => Monoid (RenderS s) where
  mempty = RenderS mempty id
  (RenderS f1 g1) `mappend` (RenderS f2 g2) = RenderS (f1 `mappend` f2) (g1 . g2)

-- Has bad performance. This instance exists only for testing purposes
instance StringLike String where
  fromChar c = [c]

{-# INLINABLE parens #-}
parens :: StringLike s => Int -> Int -> RenderS s -> RenderS s
parens p1 p2 expr = if p1 < p2 then char '(' <> expr <> char ')' else expr

#if !MIN_VERSION_base(4, 5, 0)
{-# INLINABLE (<>) #-}
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

string :: StringLike s => String -> RenderS s
string s = RenderS (fromString s) id

char :: StringLike s => Char -> RenderS s
char c = RenderS (fromChar c) id

{-# INLINABLE renderArith #-}
renderArith :: (PersistEntity v, Constructor c, StringLike s, DbDescriptor db) => Proxy db -> (s -> s) -> Arith v c a -> RenderS s
renderArith proxy escape arith = go arith 0 where
  go (Plus a b)     p = parens 6 p $ go a 6 <> char '+' <> go b 6
  go (Minus a b)    p = parens 6 p $ go a 6 <> char '-' <> go b 6
  go (Mult a b)     p = parens 7 p $ go a 7 <> char '*' <> go b 7
  go (Abs a)        p = parens 9 p $ string "ABS(" <> go a 0 <> char ')'
  go (ArithField f) _ = RenderS (head $ renderField escape f []) id
  go (Lit a)        _ = RenderS (fromChar '?') (toPurePersistValues proxy a)

{-# INLINABLE renderCond #-}
-- | Renders conditions for SQL backend. Returns Nothing if the fields don't have any columns.
renderCond :: forall v c s db . (PersistEntity v, Constructor c, StringLike s, DbDescriptor db)
  => Proxy db
  -> (s -> s) -- escape
  -> (s -> s -> s) -- render equals
  -> (s -> s -> s) -- render not equals
  -> Cond v c -> Maybe (RenderS s)
renderCond proxy esc rendEq rendNotEq (cond :: Cond v c) = go cond 0 where
  go (And a b)       p = perhaps 3 p " AND " a b
  go (Or a b)        p = perhaps 2 p " OR " a b
  go (Not a)         p = fmap (\a' -> parens 1 p $ string "NOT " <> a') $ go a 1
  go (Compare op f1 f2) p = case op of
    Eq -> renderComp 3 p " AND " rendEq f1 f2
    Ne -> renderComp 2 p " OR " rendNotEq f1 f2
    Gt -> renderComp 2 p " OR " (\a b -> a <> fromChar '>' <> b) f1 f2
    Lt -> renderComp 2 p " OR " (\a b -> a <> fromChar '<' <> b) f1 f2
    Ge -> renderComp 2 p " OR " (\a b -> a <> ">=" <> b) f1 f2
    Le -> renderComp 2 p " OR " (\a b -> a <> "<=" <> b) f1 f2

  renderComp :: Int -> Int -> s -> (s -> s -> s) -> Expr v c a -> Expr v c b -> Maybe (RenderS s)
  renderComp p pOuter logicOp op expr1 expr2 = (case expr1 of
    ExprField field -> (case expr2 of
        ExprPure  a -> guard (map (\f -> f `op` fromChar '?') fs) (toPurePersistValues proxy a)
        ExprField a -> guard (zipWith op fs $ renderField esc a []) id
        ExprArith a -> case fs of
          [f] -> let RenderS q v = renderArith proxy esc a in Just $ RenderS (f `op` q) v
          _   -> error $ "renderComp: expected one column field, found " ++ show (length fs)) where
        fs = renderField esc field []
    ExprPure pure -> (case expr2 of
      ExprPure  a -> guard (replicate (length fs) $ fromChar '?' `op` fromChar '?') (interleave fs $ toPurePersistValues proxy a [])
      ExprField a -> guard (map (\f -> fromChar '?' `op` f) $ renderField esc a []) (toPurePersistValues proxy pure)
      ExprArith a -> case fs of
        [_] -> let RenderS q v = renderArith proxy esc a in Just $ RenderS (fromChar '?' `op` q) (toPurePersistValues proxy pure . v)
        _   -> error $ "renderComp: expected one column field, found " ++ show (length fs)) where
      fs = toPurePersistValues proxy pure []
    ExprArith arith -> (case expr2 of
      ExprPure  a -> Just $ RenderS (q `op` fromChar '?') (v . toPurePersistValues proxy a) -- TODO: check list size
      ExprField a -> Just $ RenderS (q `op` head (renderField esc a [])) v -- TODO: check list size
      ExprArith a -> let RenderS q2 v2 = renderArith proxy esc a in Just $ RenderS (q `op` q2) (v . v2)) where
        RenderS q v = renderArith proxy esc arith
      ) where
        guard :: [s] -> ([PersistValue] -> [PersistValue]) -> Maybe (RenderS s)
        guard clauses values = case clauses of
          [] -> Nothing
          [clause] -> Just $ RenderS clause values
          clauses' -> Just $ parens p pOuter $ RenderS (intercalateS logicOp clauses') values
        interleave [] [] acc = acc
        interleave (x:xs) (y:ys) acc = x:y:interleave xs ys acc
        interleave _ _ _ = error "renderComp: pure values lists must have the same size"
  
  perhaps :: Int -> Int -> s -> Cond v c -> Cond v c -> Maybe (RenderS s)
  perhaps p pOuter op a b = result where
    -- we don't know if the current operator is present until we render both operands. Rendering requires priority of the outer operator. We tie a knot to defer calculating the priority
    (priority, result) = case (go a priority, go b priority) of
       (Just a', Just b') -> (p, Just $ parens p pOuter $ a' <> RenderS op id <> b')
       (Just a', Nothing) -> (pOuter, Just a')
       (Nothing, Just b') -> (pOuter, Just b')
       (Nothing, Nothing) -> (pOuter, Nothing)

{-
-- TODO: they don't support all cases
{-# INLINABLE defRenderEquals #-}
defRenderEquals :: (PersistField a, StringLike s) => (String -> String) -> Expr v c a -> Expr v c a -> RenderS s
defRenderEquals esc a b | not (isNullable a) = renderExpr esc a <> char '=' <> renderExpr esc b
-- only ExprPrim and ExprField can come here here
-- if one of arguments is Nothing, compare the other with NULL
defRenderEquals _ (ExprPure a) (ExprPure b) | isNull a && isNull b = string "NULL IS NULL"
defRenderEquals esc (ExprPure a) b | isNull a = renderExpr esc b <> string " IS NULL"
                                    | otherwise = renderPrim a <> char '=' <> renderExpr esc b
defRenderEquals esc a (ExprPure b) | isNull b = renderExpr esc a <> string " IS NULL"
                                    | otherwise = renderExpr esc a <> char '=' <> renderPrim b
--  if both are fields we compare them to each other and to null
defRenderEquals esc (ExprField a) (ExprField b) = char '(' <> a' <> char '=' <> b' <> string " OR " <> a' <> string " IS NULL AND " <> b' <> string " IS NULL)" where
  a' = string $ esc (show a)
  b' = string $ esc (show b)
defRenderEquals _ _ _ = error "for nullable values there must be no other expressions than ExprField and ExprPure"

{-# INLINABLE defRenderNotEquals #-}
defRenderNotEquals :: (PersistField a, StringLike s) => (String -> String) -> Expr v c a -> Expr v c a -> RenderS s
defRenderNotEquals esc a b | not (isNullable a) = renderExpr esc a <> string "<>" <> renderExpr esc b
-- if one of arguments is Nothing, compare the other with NULL
defRenderNotEquals _ (ExprPure a) (ExprPure b) | isNull a && isNull b = string "NULL IS NOT NULL"
defRenderNotEquals esc (ExprPure a) b | isNull a  = renderExpr esc b <> string " IS NOT NULL"
                                       | otherwise = char '(' <> renderPrim a <> string "<>" <> renderExpr esc b <> string " OR " <> renderExpr esc b <> string " IS NULL)"
defRenderNotEquals esc a (ExprPure b) | isNull b = renderExpr esc a <> string " IS NOT NULL"
                                       | otherwise = char '(' <> renderExpr esc a <> string "<>" <> renderPrim b <> string " OR " <> renderExpr esc a <> string " IS NULL)"
defRenderNotEquals esc (ExprField a) (ExprField b) = a' <> string "<>" <> b' <> string " OR (" <> a' <> string " IS NULL AND " <> b' <> string " IS NOT NULL) OR (" <> a' <> string " IS NOT NULL AND " <> b' <> string " IS NULL)" where
  a' = string $ esc (show a)
  b' = string $ esc (show b)
defRenderNotEquals _ _ _ = error "for nullable values there must be no other expressions than ExprField and ExprPure"

isNull :: Primitive a => a -> Bool
isNull a = toPrim a == PersistNull

isNullable :: PersistField a => Expr v c a -> Bool
isNullable (_ :: Expr v c a) = case dbType (undefined :: a) of
  DbMaybe _ -> True
  _         -> False

-}

renderField :: (PersistEntity v, Constructor c, FieldLike f (RestrictionHolder v c) a', StringLike s) => (s -> s) -> f -> [s] -> [s]
renderField esc field acc = renderChain esc (fieldChain field) acc

{-
examples of prefixes
[("val1", DbEmbedded False _), ("val4", EmbeddedDef False _), ("val5", EmbeddedDef False _)] -> "val5$val4$val1"
[("val1", DbEmbedded True _),  ("val4", EmbeddedDef False _), ("val5", EmbeddedDef False _)] -> ""
[("val1", DbEmbedded False _), ("val4", EmbeddedDef True _),  ("val5", EmbeddedDef False _)] -> "val1"
[("val1", DbEmbedded False _), ("val4", EmbeddedDef True _),  ("val5", EmbeddedDef True _)] -> "val1"
[("val1", DbEmbedded False _), ("val4", EmbeddedDef False _), ("val5", EmbeddedDef True _)] -> "val4$val1"
-}
{-# INLINABLE renderChain #-}
renderChain :: StringLike s => (s -> s) -> FieldChain -> [s] -> [s]
renderChain esc (f, prefix) acc = (case prefix of
  ((name, EmbeddedDef False _):fs) -> flattenP esc (goP (fromString name) fs) f acc
  _ -> flatten esc f acc) where
  goP p ((name, EmbeddedDef False _):fs) = goP (fromString name <> fromChar '$' <> p) fs
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

{-# INLINABLE renderOrders #-}
renderOrders :: forall v c s . (PersistEntity v, Constructor c, StringLike s) => (s -> s) -> [Order v c] -> s
renderOrders _ [] = mempty
renderOrders esc xs = if null orders then mempty else " ORDER BY " <> commasJoin orders where
  orders = foldr go [] xs
  go (Asc a) acc = renderField esc a acc
  go (Desc a) acc = renderField (\f -> esc f <> " DESC") a acc

{-# INLINABLE renderFields #-}
-- Returns string with comma separated escaped fields like "name,age"
-- If there are other columns before renderFields result, do not put comma because the result might be an empty string. This happens when the fields have no columns like ().
-- One of the solutions is to add one more field with datatype that is known to have columns, eg renderFields id (("id$", namedType (0 :: Int64)) : constrParams constr)
renderFields :: StringLike s => (s -> s) -> [(String, DbType)] -> s
renderFields esc = commasJoin . foldr (flatten esc) []

-- TODO: merge code of flatten and flattenP
flatten :: StringLike s => (s -> s) -> (String, DbType) -> ([s] -> [s])
flatten esc (fname, typ) acc = go typ where
  go typ' = case typ' of
    DbMaybe t -> go t
    DbEmbedded emb -> handleEmb emb
    DbEntity (Just (emb, _)) _ -> handleEmb emb
    _            -> esc fullName : acc
  fullName = fromString fname
  handleEmb (EmbeddedDef False ts) = foldr (flattenP esc fullName) acc ts
  handleEmb (EmbeddedDef True  ts) = foldr (flatten esc) acc ts

flattenP :: StringLike s => (s -> s) -> s -> (String, DbType) -> ([s] -> [s])
flattenP esc prefix (fname, typ) acc = go typ where
  go typ' = case typ' of
    DbMaybe t -> go t
    DbEmbedded emb -> handleEmb emb
    DbEntity (Just (emb, _)) _ -> handleEmb emb
    _            -> esc fullName : acc
  fullName = prefix <> fromChar '$' <> fromString fname
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

commasJoinRenders :: StringLike s => [RenderS s] -> Maybe (RenderS s)
commasJoinRenders [] = Nothing
commasJoinRenders (x:xs) = Just $ foldl' f x xs where
  f (RenderS str1 vals1) (RenderS str2 vals2) = RenderS (str1 <> comma <> str2) (vals1 <> vals2)
  comma = fromChar ','

{-# INLINABLE renderUpdates #-}
renderUpdates :: (PersistEntity v, Constructor c, StringLike s, DbDescriptor db) => Proxy db -> (s -> s) -> [Update v c] -> Maybe (RenderS s)
renderUpdates p esc = commasJoinRenders . mapMaybe go where
  go (Update field expr) = (case expr of
      ExprPure  a -> guard $ RenderS (commasJoin $ map (\f -> f <> "=?") fs) (toPurePersistValues p a)
      ExprField a -> guard $ RenderS (commasJoin $ zipWith (\f1 f2 -> f1 <> fromChar '=' <> f2) fs $ renderField esc a []) id
      ExprArith a -> case fs of
        [f] -> Just $ RenderS (f <> fromChar '=') id <> renderArith p esc a
        _   -> error $ "renderUpdates: expected one column field, found " ++ show (length fs)) where
      guard a = if null fs then Nothing else Just a
      fs = renderField esc field []

defDelim :: Char
defDelim = '$'
