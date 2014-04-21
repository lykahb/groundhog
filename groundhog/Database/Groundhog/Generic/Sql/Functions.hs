{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module has common SQL functions and operators which are supported in the most SQL databases
module Database.Groundhog.Generic.Sql.Functions
    ( like
    , notLike
    , in_
    , notIn_
    , lower
    , upper
    , case_
    , SqlDb(..)
    , cot
    , atan2
    , radians
    , degrees
    ) where

import Data.Int (Int64)
import Data.String
import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Generic.Sql

in_ :: (SqlDb db, Expression db r a, Expression db r b, PrimitivePersistField b, Unifiable a b) =>
    a -> [b] -> Cond db r
in_ _ [] = CondEmpty
in_ a bs = CondRaw $ Snippet $ \conf p -> [parens 45 p $ renderExpr conf (toExpr a) <> " IN (" <> commasJoin (map (renderExpr conf . toExpr) bs) <> ")"]

notIn_ :: (SqlDb db, Expression db r a, Expression db r b, PrimitivePersistField b, Unifiable a b) =>
       a -> [b] -> Cond db r
notIn_ _ [] = CondEmpty
notIn_ a bs = CondRaw $ Snippet $ \conf p -> [parens 45 p $ renderExpr conf (toExpr a) <> " NOT IN (" <> commasJoin (map (renderExpr conf . toExpr) bs) <> ")"]

like :: (SqlDb db, ExpressionOf db r a a', IsString a') => a -> String -> Cond db r
like a b = CondRaw $ operator 40 " LIKE " a b

notLike :: (SqlDb db, ExpressionOf db r a a', IsString a') => a -> String -> Cond db r
notLike a b = CondRaw $ operator 40 " NOT LIKE " a b

lower :: (SqlDb db, ExpressionOf db r a a', IsString a') => a -> Expr db r a'
lower a = mkExpr $ function "lower" [toExpr a]

upper :: (SqlDb db, ExpressionOf db r a a', IsString a') => a -> Expr db r a'
upper a = mkExpr $ function "upper" [toExpr a]

cot :: (FloatingSqlDb db, ExpressionOf db r a a', Floating a') => a -> Expr db r a'
cot a = mkExpr $ function "cot" [toExpr a]

radians, degrees :: (FloatingSqlDb db, ExpressionOf db r a a', Floating a') => a -> Expr db r a'
radians x = mkExpr $ function "radians" [toExpr x]
degrees x = mkExpr $ function "degrees" [toExpr x]

instance (SqlDb db, PersistField a, Num a) => Num (Expr db r a) where
  a + b = mkExpr $ operator 60 "+" a b
  a - b = mkExpr $ operator 60 "-" a b
  a * b = mkExpr $ operator 70 "*" a b
  signum a = signum' a
  abs a = mkExpr $ function "abs" [toExpr a]
  fromInteger a = Expr $ toExpr (fromIntegral a :: Int64)

instance (SqlDb db, PersistField a, Fractional a) => Fractional (Expr db r a) where
  a / b = mkExpr $ operator 70 "/" a b
  fromRational a = Expr $ toExpr (fromRational a :: Double)

instance (FloatingSqlDb db, PersistField a, Floating a) => Floating (Expr db r a) where
  pi = mkExpr $ function "pi" []
  exp x = mkExpr $ function "exp" [toExpr x]
  sqrt x = mkExpr $ function "sqrt" [toExpr x]
  log x = log' x
  x ** y = mkExpr $ function "pow" [toExpr x, toExpr y]
  logBase b x = logBase' b x
  sin x = mkExpr $ function "sin" [toExpr x]
  tan x = mkExpr $ function "tan" [toExpr x]
  cos x = mkExpr $ function "cos" [toExpr x]
  asin x = mkExpr $ function "asin" [toExpr x]
  atan x = mkExpr $ function "atan" [toExpr x]
  acos x = mkExpr $ function "acos" [toExpr x]
  sinh x = (exp x - exp (-x)) / 2
  tanh x = (exp (2 * x) - 1) / (exp (2 * x) + 1)
  cosh x = (exp x + exp (-x)) / 2
  asinh x = log $ x + sqrt (x * x + 1)
  atanh x = log ((1 + x) / (1 - x)) / 2
  acosh x = log $ x + sqrt (x * x - 1)

instance (SqlDb db, PersistField a, Ord a) => Ord (Expr db r a) where
  compare = error "compare: instance Ord (Expr db r a) does not have implementation"
  (<=) = error "(<=): instance Ord (Expr db r a) does not have implementation"
  max a b = mkExpr $ function "max" [toExpr a, toExpr b]
  min a b = mkExpr $ function "min" [toExpr a, toExpr b]

instance (SqlDb db, PersistField a, Real a) => Real (Expr db r a) where
  toRational = error "toRational: instance Real (Expr db r a) is made only for Integral superclass constraint"

instance (SqlDb db, PersistField a, Enum a) => Enum (Expr db r a) where
  toEnum = error "toEnum: instance Enum (Expr db r a) is made only for Integral superclass constraint"
  fromEnum = error "fromEnum: instance Enum (Expr db r a) is made only for Integral superclass constraint"

instance (SqlDb db, PurePersistField a, Integral a) => Integral (Expr db r a) where
  quotRem x y = quotRem' x y
  divMod x y = (div', mod') where
    div' = mkExprWithConf $ \conf _ -> let
             x' = prerenderExpr conf x
             y' = prerenderExpr conf y
       in case_ [ (x' >. zero &&. y' <. zero, (x' - y' - 1) `quot` y')
                , (x' <. zero &&. y' >. zero, (x' - y' + 1) `quot` y')
                ] (x' `quot` y')
    mod' = mkExprWithConf $ \conf _ -> let
             x' = prerenderExpr conf x
             y' = prerenderExpr conf y
       in case_ [ (x' >. zero &&. y' <. zero ||. x' <. zero &&. y' >. zero,
                   case_ [((x' `rem` y') /=. zero, x' `rem` y' + y')] zero)
                ] (x' `rem` y')
    zero = 0 `asTypeOf` ((undefined :: Expr db r a -> a) x)
  toInteger = error "toInteger: instance Integral (Expr db r a) does not have implementation"

case_ :: (SqlDb db, ExpressionOf db r a a', ExpressionOf db r b a')
      => [(Cond db r, a)] -- ^ Conditions
      -> b          -- ^ It is returned when none of conditions is true
      -> Expr db r a'
case_ [] else_ = Expr $ toExpr else_
case_ cases else_ = mkExpr $ Snippet $ \conf _ ->
     ["case "
  <> intercalateS (fromChar ' ') (map (rend conf) cases)
  <> " else " <> renderExpr conf (toExpr else_)
  <> " end"] where
  rend conf (cond, a) = case renderCond conf cond of
    Nothing -> error "case_: empty condition"
    Just cond' -> "when " <> cond' <> " then " <> renderExpr conf (toExpr a)
