{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings, UndecidableInstances #-}
module Database.Groundhog.Postgresql.Array
  (
    Array(..)
  , (!)
  , (!:)
  , arrayDims
  , arrayUpper
  , arrayLower
  , arrayLength
  ) where

import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Postgresql

-- | Represents PostgreSQL arrays
newtype Array a = Array [a]

instance PersistField a => PersistField (Array a) where
  persistName a = "Array" ++ delim : delim : persistName ((undefined :: Array a -> a) a)
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType a = DbOther $ OtherTypeDef $ \f -> f elemType ++ "[]" where
    elemType = dbType ((undefined :: Array a -> a) a)

instance PersistField a => PrimitivePersistField (Array a) where

(!) :: (ExpressionOf Postgresql r a (Array elem), ExpressionOf Postgresql r b Int) => a -> b -> Expr Postgresql r elem
(!) arr i = Expr $ Snippet $ \esc _ -> [renderExpr esc (toExpr arr) <> "[" <> renderExpr esc (toExpr i) <> ":" <> renderExpr esc (toExpr i) <> "]"]

(!:) :: (ExpressionOf Postgresql r a (Array elem), ExpressionOf Postgresql r i1 Int, ExpressionOf Postgresql r i2 Int) => a -> (i1, i2) -> Expr Postgresql r elem
(!:) arr (i1, i2) = Expr $ Snippet $ \esc _ -> [renderExpr esc (toExpr arr) <> "[" <> renderExpr esc (toExpr i1) <> ":" <> renderExpr esc (toExpr i2) <> "]"]

-- arr ! 2 !: (3, 4)

arrayDims :: (ExpressionOf Postgresql r a (Array elem)) => a -> Expr Postgresql r String
arrayDims arr = Expr $ Snippet $ \esc _ -> ["array_dims(" <> renderExpr esc (toExpr arr) <> ")"]

arrayUpper :: (ExpressionOf Postgresql r a (Array elem)) => a -> Int -> Expr Postgresql r String
arrayUpper arr dim = Expr $ Snippet $ \esc _ -> ["array_upper(" <> renderExpr esc (toExpr arr) <> fromString (show dim ++ ")")]

arrayLower :: (ExpressionOf Postgresql r a (Array elem)) => a -> Int -> Expr Postgresql r String
arrayLower arr dim = Expr $ Snippet $ \esc _ -> ["array_lower(" <> renderExpr esc (toExpr arr) <> fromString (show dim ++ ")")]

arrayLength :: (ExpressionOf Postgresql r a (Array elem)) => a -> Int -> Expr Postgresql r String
arrayLength arr dim = Expr $ Snippet $ \esc _ -> ["array_length(" <> renderExpr esc (toExpr arr) <> fromString (show dim ++ ")")]
