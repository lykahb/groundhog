{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, FunctionalDependencies, UndecidableInstances, OverlappingInstances, EmptyDataDecls #-}

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
import Database.Groundhog.Instances ()

-- | Instances of this type can be converted to 'Expr'. It is useful for uniform manipulation over fields and plain values
class Expression a v c where
  wrap :: a -> Expr v c a

instance PurePersistField a => Expression a v c where
  wrap = ExprPure

instance (PersistEntity v, Constructor c, PersistField a, v ~ v', c ~ c') => Expression (Arith v c a) v' c' where
  wrap = ExprArith

instance (PersistEntity v, Constructor c, PersistField a, v ~ v', c ~ c') => Expression (Field v c a) v' c' where
  wrap = ExprField

instance (PersistEntity v, Constructor c, PersistField a, v ~ v', c ~ c') => Expression (SubField v c a) v' c' where
  wrap = ExprField

instance (PersistEntity v, Constructor c, PersistField (Key v' BackendSpecific), FieldLike (AutoKeyField v c) (RestrictionHolder v c) a', v ~ v', c ~ c') => Expression (AutoKeyField v c) v' c' where
  wrap = ExprField

instance (PersistEntity v, Constructor c, FieldLike (u (UniqueMarker v)) (RestrictionHolder v c) a', c' ~ UniqueConstr (Key v' (Unique u)), v ~ v', IsUniqueKey (Key v' (Unique u)), c ~ c') => Expression (u (UniqueMarker v)) v' c' where
  wrap = ExprField

-- Let's call "plain type" the types that uniquely define type of a Field it is compared to.
-- Example: Int -> Field v c Int, but Entity -> Field v c (Entity / Key Entity)
class Unifiable a b
instance Unifiable a a
-- Tie a type-level knot. Knowing if another type is plain helps to avoid indirection. In practice, it enables to infer type of polymorphic field when it is compared to a plain type.
instance (Normalize bk a (ak, r), Normalize ak b (bk, r)) => Unifiable a b

class Normalize counterpart t r | t -> r
instance (ExtractValue t (isPlain, r), NormalizeValue counterpart isPlain r r') => Normalize counterpart t r'

class ExtractValue t r | t -> r
instance r ~ (HFalse, a) => ExtractValue (Arith v c a) r
instance r ~ (HFalse, a) => ExtractValue (Field v c a) r
instance r ~ (HFalse, a) => ExtractValue (SubField v c a) r
instance r ~ (HFalse, Key v BackendSpecific) => ExtractValue (AutoKeyField v c) r
instance r ~ (HFalse, Key v (Unique u)) => ExtractValue (u (UniqueMarker v)) r
instance r ~ (HTrue, a) => ExtractValue a r

class NormalizeValue counterpart isPlain t r | t isPlain -> r
instance NormalizeValue' t (isPlain, r) => NormalizeValue HFalse HFalse t (HFalse, r)
instance NormalizeValue' t (isPlain, r) => NormalizeValue HFalse HTrue t (isPlain, r)
instance r ~ (isPlain, t) => NormalizeValue HTrue isPlain t r

class NormalizeValue' t r | t -> r
-- Normalize @Key v u@ to @v@ only if this key is used for storing @v@.
instance (TypeEq (DefaultKey a) (Key a u) isDef,
         r ~ (Not isDef, Maybe (NormalizeKey isDef (Key a u))))
         => NormalizeValue' (Maybe (Key a u)) r
instance (TypeEq (DefaultKey a) (Key a u) isDef,
         r ~ (Not isDef, NormalizeKey isDef (Key a u)))
         => NormalizeValue' (Key a u) r
instance r ~ (HTrue, a) => NormalizeValue' a r

class TypeEq x y b | x y -> b
instance (b ~ HFalse) => TypeEq x y b	 
instance TypeEq x x HTrue

type family NormalizeKey isDef key
type instance NormalizeKey HTrue (Key a u) = a
type instance NormalizeKey HFalse a = a

type family Not bool
type instance Not HTrue  = HFalse
type instance Not HFalse = HTrue

-- | Update field
infixr 3 =.
(=.) ::
  ( Expression b v c
  , FieldLike f (RestrictionHolder v c) a'
  , Unifiable f b)
  => f -> b -> Update v c
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
