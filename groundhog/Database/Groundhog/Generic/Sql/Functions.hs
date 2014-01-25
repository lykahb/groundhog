{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings #-}

-- | This module has common SQL functions and operators which are supported in the most SQL databases
module Database.Groundhog.Generic.Sql.Functions
    ( like
    , notLike
    , in_
    , notIn_
    , lower
    , upper
    , toArith
    , SqlDb(..)
    ) where

import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Generic.Sql

in_ :: (SqlDb db, QueryRaw db ~ Snippet db, Expression db r a, Expression db r b, PrimitivePersistField b, Unifiable a b) =>
    a -> [b] -> Cond db r
in_ _ [] = CondEmpty
in_ a bs = CondRaw $ Snippet $ \esc p -> [parens 45 p $ renderExpr esc (toExpr a) <> " IN (" <> commasJoin (map (renderExpr esc . toExpr) bs) <> ")"]

notIn_ :: (SqlDb db, QueryRaw db ~ Snippet db, Expression db r a, Expression db r b, PrimitivePersistField b, Unifiable a b) =>
       a -> [b] -> Cond db r
notIn_ _ [] = CondEmpty
notIn_ a bs = CondRaw $ Snippet $ \esc p -> [parens 45 p $ renderExpr esc (toExpr a) <> " NOT IN (" <> commasJoin (map (renderExpr esc . toExpr) bs) <> ")"]

like :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r a String) => a -> String -> Cond db r
like a b = CondRaw $ operator 40 " LIKE " a b

notLike :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r a String) => a -> String -> Cond db r
notLike a b = CondRaw $ operator 40 " NOT LIKE " a b

lower :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r a String) => a -> Expr db r String
lower a = Expr $ function "lower" [toExpr a]

upper :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r a String) => a -> Expr db r String
upper a = Expr $ function "upper" [toExpr a]

-- | Convert field to an arithmetic value. It is kept for compatibility with older Groundhog versions and can be replaced with liftExpr.
toArith :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r f a', FieldLike f db r a') => f -> Expr db r a'
toArith = liftExpr
