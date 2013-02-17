{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings #-}

-- | This module defines the functions which are used only for backends creation.
module Database.Groundhog.Generic.Sql.Functions
    ( like
    , in_
    , notIn_
    , lower
    , upper
    , append
    , toArith
    ) where

import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Generic.Sql

in_ :: (SqlDb db, QueryRaw db ~ Snippet db, Expression db r a, Expression db r b, PrimitivePersistField b, Unifiable a b) =>
    a -> [b] -> Cond db r
in_ a bs = CondRaw $ Snippet $ \esc _ -> [renderExpr esc (toExpr a) <> " IN (" <> intercalateS (fromChar ',') (map (renderExpr esc . toExpr) bs) <> ")"]

notIn_ :: (SqlDb db, QueryRaw db ~ Snippet db, Expression db r a, Expression db r b, PrimitivePersistField b, Unifiable a b) =>
    a -> [b] -> Cond db r
notIn_ a bs = CondRaw $ Snippet $ \esc _ -> [renderExpr esc (toExpr a) <> " IN (" <> intercalateS (fromChar ',') (map (renderExpr esc . toExpr) bs) <> ")"]

like :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r a String) => a -> String -> Cond db r
like a b = CondRaw $ Snippet $ \esc _ -> [renderExpr esc (toExpr a) <> " LIKE " <> renderExpr esc (toExpr b)]

lower :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r a String) => a -> Expr db r String
lower a = Expr $ Snippet $ \esc _ -> ["lower(" <> renderExpr esc (toExpr a) <> ")"]

upper :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r a String) => a -> Expr db r String
upper a = Expr $ Snippet $ \esc _ -> ["upper(" <> renderExpr esc (toExpr a) <> ")"]

append :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r a String, ExpressionOf db r b String) => a -> b -> Expr db r String
append a b = Expr $ Snippet $ \esc _ -> [renderExpr esc (toExpr a) <> "||" <> renderExpr esc (toExpr b)]

-- | Convert field to an arithmetic value. It is kept for compatibility with older Groundhog versions and can be replaced with liftExpr.
toArith :: (SqlDb db, QueryRaw db ~ Snippet db, ExpressionOf db r f a', FieldLike f db r a') => f -> Expr db r a'
toArith = liftExpr
