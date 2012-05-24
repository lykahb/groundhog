{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FunctionalDependencies, UndecidableInstances, OverlappingInstances, EmptyDataDecls #-}

-- | This module provides mechanism for flexible and typesafe usage of plain data values and fields.
-- The expressions can used in conditions and right part of Update statement.
-- Example:
--
-- @
-- StringField ==. \"abc\" &&. NumberField >. (0 :: Int) ||. MaybeField ==. (Nothing :: Maybe String) ||. MaybeField ==. Just \"def\"
-- @
--
-- Note that polymorphic values like numbers or Nothing must have a type annotation

module Database.Groundhog.Expression
  ( Expression(..)
  , (=.)
  , (&&.), (||.)
  , (==.), (/=.), (<.), (<=.), (>.), (>=.)
  ) where

import Database.Groundhog.Core

-- | Instances of this type can be converted to 'Expr'. It is useful for uniform manipulation over fields and plain values
class Expression a v c where
  wrap :: a -> Expr v c a

instance PurePersistField a => Expression a v c where
  wrap = ExprPure

instance (PersistEntity v, v ~ v', c ~ c') => Expression (Field v c a) v' c' where
  wrap = ExprField

instance (PersistEntity v, v ~ v', c ~ c') => Expression (SubField v c a) v' c' where
  wrap = ExprField

instance (PersistEntity v, v ~ v', c ~ c') => Expression (Arith v c a) v' c' where
  wrap = ExprArith

data HTrue
data HFalse

-- Let's call "plain type" the types that uniquely define type of a Field it is compared to.
-- Example: Int -> Field v c Int, but Entity -> Field v c (Entity / Key Entity)
class Unifiable a b
instance Unifiable a a
-- Tie a type-level knot. Knowing if another type is plain helps to avoid indirection. In practice, it enables to infer type of polymorphic field when it is compared to a plain type.
instance (Normalize bk a (ak, r), Normalize ak b (bk, r)) => Unifiable a b

class Normalize counterpart t r | t -> r
instance (ExtractValue t (isPlain, r), NormalizeValue counterpart isPlain r r') => Normalize counterpart t r'

class NormalizeValue counterpart isPlain t r | t isPlain -> r
instance NormalizeValue' t (isPlain, r) => NormalizeValue HFalse HFalse t (HFalse, r)
instance NormalizeValue' t (isPlain, r) => NormalizeValue HFalse HTrue t (isPlain, r)
instance r ~ (isPlain, t) => NormalizeValue HTrue isPlain t r

class NormalizeValue' t r | t -> r
instance r ~ (HFalse, Maybe a) => NormalizeValue' (Maybe (Key a)) r
instance r ~ (HFalse, a) => NormalizeValue' (Key a) r
instance r ~ (HTrue, a) => NormalizeValue' a r

class ExtractValue t r | t -> r
instance r ~ (HFalse, a) => ExtractValue (Field v c a) r
instance r ~ (HFalse, a) => ExtractValue (SubField v c a) r
instance r ~ (HTrue, a) => ExtractValue (Arith v c a) r
instance r ~ (HTrue, a) => ExtractValue a r

-- | Update field
infixr 3 =.
(=.) ::
  ( Expression b v c
  , FieldLike f
  , Unifiable (f v c a) b)
  => f v c a -> b -> Update v c
f =. b = Update f (wrap b)

-- | Boolean \"and\" operator.
(&&.) :: Cond v c -> Cond v c -> Cond v c

-- | Boolean \"or\" operator.  
(||.) :: Cond v c -> Cond v c -> Cond v c

infixr 3 &&.
a &&. b = And a b

infixr 2 ||.
a ||. b = Or a b

(==.), (/=.) ::
  ( Expression a v c
  , Expression b v c
  , Unifiable a b)
  => a -> b -> Cond v c

(<.), (<=.), (>.), (>=.) ::
  ( Expression a v c
  , Expression b v c
  , Unifiable a b)
  => a -> b -> Cond v c

infix 4 ==., <., <=., >., >=.
a ==. b = Compare Eq (wrap a) (wrap b)
a /=. b = Compare Ne (wrap a) (wrap b)
a <.  b = Compare Lt (wrap a) (wrap b)
a <=. b = Compare Le (wrap a) (wrap b)
a >.  b = Compare Gt (wrap a) (wrap b)
a >=. b = Compare Ge (wrap a) (wrap b)
