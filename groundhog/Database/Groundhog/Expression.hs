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
  , Unifiable
  , ExpressionOf
  , (=.)
  , (&&.), (||.)
  , (==.), (/=.), (<.), (<=.), (>.), (>=.)
  ) where

import Database.Groundhog.Core
import Database.Groundhog.Instances ()

-- | Instances of this type can be converted to 'UntypedExpr'. It is useful for uniform manipulation over fields, constant values, etc.
class Expression db r a where
  toExpr :: a -> UntypedExpr db r

-- | This helper class can make type signatures more concise
class (Expression db r a, Unifiable a a') => ExpressionOf db r a a'

instance (Expression db r a, Unifiable a a') => ExpressionOf db r a a'

instance PurePersistField a => Expression db r a where
  toExpr = ExprPure

instance (PersistField a, db' ~ db, r' ~ r) => Expression db' r' (Expr db r a) where
  toExpr = ExprRaw

instance (PersistEntity v, Constructor c, PersistField a, RestrictionHolder v c ~ r') => Expression db r' (Field v c a) where
  toExpr = ExprField . fieldChain

instance (PersistEntity v, Constructor c, PersistField a, RestrictionHolder v c ~ r') => Expression db r' (SubField v c a) where
  toExpr = ExprField . fieldChain

instance (PersistEntity v, Constructor c, RestrictionHolder v c ~ r') => Expression db r' (AutoKeyField v c) where
  toExpr = ExprField . fieldChain

instance (PersistEntity v, IsUniqueKey k, k ~ Key v (Unique u), RestrictionHolder v (UniqueConstr k) ~ r')
      => Expression db r' (u (UniqueMarker v)) where
  toExpr = ExprField . fieldChain

-- Let's call "plain type" the types that uniquely define type of a Field it is compared to.
-- Example: Int -> Field v c Int, but Entity -> Field v c (Entity / Key Entity)
class Unifiable a b
instance Unifiable a a
-- Tie a type-level knot. Knowing if another type is plain helps to avoid indirection. In practice, it enables to infer type of polymorphic field when it is compared to a plain type.
instance (Normalize bk a (ak, r), Normalize ak b (bk, r)) => Unifiable a b

class Normalize counterpart t r | t -> r
instance (ExtractValue t (isPlain, r), NormalizeValue counterpart isPlain r r') => Normalize counterpart t r'

class ExtractValue t r | t -> r
instance r ~ (HFalse, a) => ExtractValue (Field v c a) r
instance r ~ (HFalse, a) => ExtractValue (SubField v c a) r
instance r ~ (HFalse, a) => ExtractValue (Expr db r' a) r
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
  ( FieldLike f db r a'
  , Expression db r b
  , Unifiable f b)
  => f -> b -> Update db r
f =. b = Update f (toExpr b)

-- | Boolean \"and\" operator.
(&&.) :: Cond db r -> Cond db r -> Cond db r

-- | Boolean \"or\" operator.
(||.) :: Cond db r -> Cond db r -> Cond db r

infixr 3 &&.
a &&. b = And a b

infixr 2 ||.
a ||. b = Or a b

(==.), (/=.) ::
  ( Expression db r a
  , Expression db r b
  , Unifiable a b)
  => a -> b -> Cond db r

(<.), (<=.), (>.), (>=.) ::
  ( Expression db r a
  , Expression db r b
  , Unifiable a b)
  => a -> b -> Cond db r

infix 4 ==., <., <=., >., >=.
a ==. b = Compare Eq (toExpr a) (toExpr b)
a /=. b = Compare Ne (toExpr a) (toExpr b)
a <.  b = Compare Lt (toExpr a) (toExpr b)
a <=. b = Compare Le (toExpr a) (toExpr b)
a >.  b = Compare Gt (toExpr a) (toExpr b)
a >=. b = Compare Ge (toExpr a) (toExpr b)
