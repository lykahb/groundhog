{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, Rank2Types, GADTs #-}

-- | This module defines the functions which are used only for backends creation.
module Database.Groundhog.Generic.Sql
    ( renderCond
    , defaultShowPrim
    , renderArith
    , renderOrders
    , renderUpdates
    , defId
    , defDelim
    , defRenderEquals
    , defRenderNotEquals
    , renderExpr
    , RenderS
    , (<>)
    ) where

import Database.Groundhog.Core

parens :: Int -> Int -> RenderS -> RenderS
parens p1 p2 expr = if p1 < p2 then char '(' <> expr <> char ')' else expr

(<>) :: RenderS -> RenderS -> RenderS
(f1, g1) <> (f2, g2) = (f1.f2, g1.g2)

string :: String -> RenderS
string s = ((s++), id)

char :: Char -> RenderS
char c = ((c:), id)
  
type RenderS = (ShowS, [PersistValue] -> [PersistValue])

renderArith :: PersistEntity v => (String -> String) -> Arith v c a -> RenderS
renderArith escape arith = go arith 0 where
  go :: PersistEntity v => Arith v c a -> Int -> RenderS
  go (Plus a b)     p = parens 6 p $ go a 6 <> char '+' <> go b 6
  go (Minus a b)    p = parens 6 p $ go a 6 <> char '-' <> go b 6
  go (Mult a b)     p = parens 7 p $ go a 7 <> char '*' <> go b 7
  go (Abs a)        p = parens 9 p $ string "ABS(" <> go a 0 <> char ')'
  go (ArithField f) _ = string (escape (show f))
  go (Lit a)        _ = (('?':), (toPrim a:))

renderCond :: PersistEntity v
  => (String -> String) -- escape
  -> String -- name of id in constructor table
  -> (forall a.PersistField a => (String -> String) -> Expr v c a -> Expr v c a -> RenderS) -- render equals
  -> (forall a.PersistField a => (String -> String) -> Expr v c a -> Expr v c a -> RenderS) -- render not equals
  -> Cond v c -> RenderS
renderCond esc idName rendEq rendNotEq (cond :: Cond v c) = go cond 0 where
  go :: Cond v c -> Int -> RenderS
  go (And a b)       p = parens 3 p $ go a 3 <> string " AND " <> go b 3
  go (Or a b)        p = parens 2 p $ go a 2 <> string " OR " <> go b 2
  go (Not a)         p = parens 1 p $ string "NOT " <> go a 1
  -- the comparisons have the highest priority, so they never need parentheses
  go (Lesser a b)    _ = renderExpr esc a <> char '<' <> renderExpr esc b
  go (Greater a b)   _ = renderExpr esc a <> char '>' <> renderExpr esc b
  go (Equals a b)    _ = rendEq esc a b
  go (NotEquals a b) _ = rendNotEq esc a b
  go (KeyIs k) _ = ((idName ++) . ("=?" ++), (toPrim k:))

-- TODO: they don't support all cases
defRenderEquals :: PersistField a => (String -> String) -> Expr v c a -> Expr v c a -> RenderS
defRenderEquals esc a b | not (isNullable a) = renderExpr esc a <> char '=' <> renderExpr esc b
-- only ExprPrim and ExprField can come here here
-- if one of arguments is Nothing, compare the other with NULL
defRenderEquals _ (ExprPlain a) (ExprPlain b) | isNull a && isNull b = string "NULL IS NULL"
defRenderEquals esc (ExprPlain a) b | isNull a = renderExpr esc b <> string " IS NULL"
                                    | otherwise = renderPrim a <> char '=' <> renderExpr esc b
defRenderEquals esc a (ExprPlain b) | isNull b = renderExpr esc a <> string " IS NULL"
                                    | otherwise = renderExpr esc a <> char '=' <> renderPrim b
--  if both are fields we compare them to each other and to null
defRenderEquals esc (ExprField a) (ExprField b) = char '(' <> a' <> char '=' <> b' <> string " OR " <> a' <> string " IS NULL AND " <> b' <> string " IS NULL)" where
  a' = string $ esc (show a)
  b' = string $ esc (show b)
defRenderEquals _ _ _ = error "for nullable values there must be no other expressions than ExprField and ExprPlain"

defRenderNotEquals :: PersistField a => (String -> String) -> Expr v c a -> Expr v c a -> RenderS
defRenderNotEquals esc a b | not (isNullable a) = renderExpr esc a <> string "<>" <> renderExpr esc b
-- if one of arguments is Nothing, compare the other with NULL
defRenderNotEquals _ (ExprPlain a) (ExprPlain b) | isNull a && isNull b = string "NULL IS NOT NULL"
defRenderNotEquals esc (ExprPlain a) b | isNull a  = renderExpr esc b <> string " IS NOT NULL"
                                       | otherwise = char '(' <> renderPrim a <> string "<>" <> renderExpr esc b <> string " OR " <> renderExpr esc b <> string " IS NULL)"
defRenderNotEquals esc a (ExprPlain b) | isNull b = renderExpr esc a <> string " IS NOT NULL"
                                       | otherwise = char '(' <> renderExpr esc a <> string "<>" <> renderPrim b <> string " OR " <> renderExpr esc a <> string " IS NULL)"
defRenderNotEquals esc (ExprField a) (ExprField b) = a' <> string "<>" <> b' <> string " OR (" <> a' <> string " IS NULL AND " <> b' <> string " IS NOT NULL) OR (" <> a' <> string " IS NOT NULL AND " <> b' <> string " IS NULL)" where
  a' = string $ esc (show a)
  b' = string $ esc (show b)
defRenderNotEquals _ _ _ = error "for nullable values there must be no other expressions than ExprField and ExprPlain"

isNull :: Primitive a => a -> Bool
isNull a = toPrim a == PersistNull

renderExpr :: (String -> String) -> Expr v c a -> RenderS
renderExpr esc (ExprField a) = string $ esc (show a)
renderExpr _   (ExprPrim a)  = (('?':), (toPrim a:))
renderExpr _   (ExprPlain a)  = (('?':), (toPrim a:))
renderExpr esc (ExprArith a) = renderArith esc a

renderPrim :: Primitive a => a -> RenderS
renderPrim a = (('?':), (toPrim a:))

isNullable :: PersistField a => Expr v c a -> Bool
isNullable (_ :: Expr v c a) = case dbType (undefined :: a) of
  DbMaybe _ -> True
  _         -> False

defaultShowPrim :: PersistValue -> String
defaultShowPrim (PersistString x) = "'" ++ x ++ "'"
defaultShowPrim (PersistByteString x) = "'" ++ show x ++ "'"
defaultShowPrim (PersistInt64 x) = show x
defaultShowPrim (PersistDouble x) = show x
defaultShowPrim (PersistBool x) = if x then "1" else "0"
defaultShowPrim (PersistDay x) = show x
defaultShowPrim (PersistTimeOfDay x) = show x
defaultShowPrim (PersistUTCTime x) = show x
defaultShowPrim (PersistNull) = "NULL"

renderOrders :: PersistEntity v => (String -> String) -> [Order v c] -> ShowS
renderOrders _ [] = id
renderOrders esc (x:xs) = (" ORDER BY " ++) . f x . rest where
  rest = foldr (\ord r -> (',':) . f ord . r) id xs
  f (Asc a)  = (esc (show a) ++)
  f (Desc a) = (esc (show a) ++) . (" DESC" ++)

renderUpdates :: PersistEntity v => (String -> String) -> [Update v c] -> RenderS
renderUpdates _ [] = (id, id)
renderUpdates esc (x:xs) = f x <> rest where
  rest = foldr (\ord r -> char ',' <> f ord <> r) (id, id) xs
  f (Update field a) = string (esc (show field)) <> char '=' <> renderExpr esc a

defId :: String
defId = "id$"

defDelim :: Char
defDelim = '$'