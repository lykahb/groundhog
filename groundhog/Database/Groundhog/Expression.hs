{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides mechanism for flexible and typesafe usage of plain data values and fields.
-- The expressions can used in conditions and right part of Update statement.
-- Example:
--
-- @
-- StringField ==. \"abc\" &&. NumberField >. (0 :: Int) ||. MaybeField ==. (Nothing :: Maybe String) ||. MaybeField ==. Just \"def\"
-- @
--
-- Note that polymorphic values like numbers or Nothing must have a type annotation.
-- Comparison operators specific for SQL such as IN and LIKE are defined in "Database.Groundhog.Generic.Sql.Functions".
module Database.Groundhog.Expression
  ( Expression (..),
    Unifiable,
    ExpressionOf,
    (=.),
    (&&.),
    (||.),
    (==.),
    (/=.),
    (<.),
    (<=.),
    (>.),
    (>=.),
    isFieldNothing,
    liftExpr,
    toArith,
  )
where

import Database.Groundhog.Core
import Database.Groundhog.Instances ()

-- | Instances of this type can be converted to 'UntypedExpr'. It is useful for uniform manipulation over fields, constant values, etc.
class Expression db r a where
  toExpr :: a -> UntypedExpr db r

fieldHelper :: (FieldLike f a, DbDescriptor db, ProjectionDb f db) => f -> UntypedExpr db r
fieldHelper f = result
  where
    result = ExprField $ fieldChain db f
    db = (undefined :: UntypedExpr db r -> proxy db) result

instance {-# OVERLAPPING #-} (EntityConstr v c, DbDescriptor db, PersistField a, RestrictionHolder v c ~ r') => Expression db r' (Field v c a) where
  toExpr = fieldHelper

instance {-# OVERLAPPING #-} (EntityConstr v c, DbDescriptor db, PersistField a, db' ~ db, RestrictionHolder v c ~ r') => Expression db' r' (SubField db v c a) where
  toExpr = fieldHelper

instance {-# OVERLAPPING #-} (EntityConstr v c, DbDescriptor db, RestrictionHolder v c ~ r') => Expression db r' (AutoKeyField v c) where
  toExpr = fieldHelper

instance
  {-# OVERLAPPING #-}
  (PersistEntity v, DbDescriptor db, IsUniqueKey k, k ~ Key v (Unique u), RestrictionHolder v c ~ r') =>
  Expression db r' (u (UniqueMarker v))
  where
  toExpr = fieldHelper

instance {-# OVERLAPPING #-} (db' ~ db, r' ~ r) => Expression db' r' (Cond db r) where
  toExpr = ExprCond

-- Let's call "plain type" the types that uniquely define type of a Field it is compared to.
-- Example: Int -> Field v c Int, but Entity -> Field v c (Entity / Key Entity)
class Unifiable a b

instance {-# OVERLAPPING #-} Unifiable a a

-- Tie a type-level knot. Knowing if another type is plain helps to avoid indirection. In practice, it enables to infer type of polymorphic field when it is compared to a plain type.
instance {-# OVERLAPPABLE #-} (Normalize bk a (ak, r), Normalize ak b (bk, r)) => Unifiable a b

-- | This helper class can make type signatures more concise
class (Expression db r a, PersistField a') => ExpressionOf db r a a' | a -> a'

instance (Expression db r a, Normalize HTrue a (flag, a'), PersistField a') => ExpressionOf db r a a'

instance {-# OVERLAPPABLE #-} PurePersistField a => Expression db r a where
  toExpr = ExprPure

instance {-# OVERLAPPING #-} (PersistField a, db' ~ db, r' ~ r) => Expression db' r' (Expr db r a) where
  toExpr (Expr e) = e

class Normalize counterpart t r | t -> r

instance {-# OVERLAPPING #-} NormalizeValue a (isPlain, r) => Normalize HFalse (Field v c a) (HFalse, r)

instance {-# OVERLAPS #-} r ~ (HFalse, a) => Normalize HTrue (Field v c a) r

instance {-# OVERLAPPING #-} NormalizeValue a (isPlain, r) => Normalize HFalse (SubField db v c a) (HFalse, r)

instance {-# OVERLAPS #-} r ~ (HFalse, a) => Normalize HTrue (SubField db v c a) r

instance {-# OVERLAPPING #-} NormalizeValue a (isPlain, r) => Normalize HFalse (Expr db r' a) (HFalse, r)

instance {-# OVERLAPS #-} r ~ (HFalse, a) => Normalize HTrue (Expr db r' a) r

instance {-# OVERLAPPING #-} NormalizeValue (Key v (Unique u)) (isPlain, r) => Normalize HFalse (u (UniqueMarker v)) (HFalse, r)

instance {-# OVERLAPS #-} r ~ (HFalse, Key v (Unique u)) => Normalize HTrue (u (UniqueMarker v)) r

instance {-# OVERLAPPING #-} NormalizeValue (Key v BackendSpecific) (isPlain, r) => Normalize HFalse (AutoKeyField v c) (HFalse, r)

instance {-# OVERLAPS #-} r ~ (HFalse, Key v BackendSpecific) => Normalize HTrue (AutoKeyField v c) r

instance {-# OVERLAPPING #-} r ~ (HTrue, Bool) => Normalize HFalse (Cond db r') r

instance {-# OVERLAPPING #-} r ~ (HTrue, Bool) => Normalize HTrue (Cond db r') r

instance {-# OVERLAPPABLE #-} NormalizeValue t r => Normalize HFalse t r

instance {-# OVERLAPPABLE #-} r ~ (HTrue, t) => Normalize HTrue t r

class NormalizeValue t r | t -> r

-- Normalize @Key v u@ to @v@ only if this key is used for storing @v@.

instance
  {-# OVERLAPPING #-}
  ( TypeEq (DefaultKey v) (Key v u) isDef,
    NormalizeKey isDef v u k,
    r ~ (Not isDef, Maybe k)
  ) =>
  NormalizeValue (Maybe (Key v u)) r

instance
  {-# OVERLAPPING #-}
  ( TypeEq (DefaultKey v) (Key v u) isDef,
    NormalizeKey isDef v u k,
    r ~ (Not isDef, k)
  ) =>
  NormalizeValue (Key v u) r

instance {-# OVERLAPPABLE #-} r ~ (HTrue, a) => NormalizeValue a r

class TypeEq x y b | x y -> b

instance {-# OVERLAPPABLE #-} b ~ HFalse => TypeEq x y b

instance {-# OVERLAPPING #-} TypeEq x x HTrue

class NormalizeKey isDef v u k | isDef v u -> k, k -> v

instance k ~ v => NormalizeKey HTrue v u k

instance k ~ Key v u => NormalizeKey HFalse v u k

type family Not bool

type instance Not HTrue = HFalse

type instance Not HFalse = HTrue

-- | Update field
infixr 3 =.

(=.) ::
  ( Assignable f a',
    ProjectionDb f db,
    ProjectionRestriction f r,
    Expression db r b,
    Unifiable f b
  ) =>
  f ->
  b ->
  Update db r
f =. b = Update f (toExpr b)

-- | Boolean \"and\" operator.
(&&.) :: Cond db r -> Cond db r -> Cond db r

-- | Boolean \"or\" operator.
(||.) :: Cond db r -> Cond db r -> Cond db r

infixr 3 &&.

a &&. b = And a b

infixr 2 ||.

a ||. b = Or a b

(==.),
  (/=.) ::
    ( Expression db r a,
      Expression db r b,
      Unifiable a b
    ) =>
    a ->
    b ->
    Cond db r
(<.),
  (<=.),
  (>.),
  (>=.) ::
    ( Expression db r a,
      Expression db r b,
      Unifiable a b
    ) =>
    a ->
    b ->
    Cond db r

infix 4 ==., /=., <., <=., >., >=.

a ==. b = Compare Eq (toExpr a) (toExpr b)

a /=. b = Compare Ne (toExpr a) (toExpr b)

a <. b = Compare Lt (toExpr a) (toExpr b)

a <=. b = Compare Le (toExpr a) (toExpr b)

a >. b = Compare Gt (toExpr a) (toExpr b)

a >=. b = Compare Ge (toExpr a) (toExpr b)

-- | This function more limited than (==.), but has better type inference.
-- If you want to compare your value to Nothing with @(==.)@ operator, you have to write the types explicitly @myExpr ==. (Nothing :: Maybe Int)@.
-- TODO: restrict db r
isFieldNothing :: (Expression db r f, Projection f (Maybe a), PrimitivePersistField (Maybe a), Unifiable f (Maybe a)) => f -> Cond db r
isFieldNothing a = a `eq` Nothing
  where
    eq :: (Expression db r f, Expression db r a, Projection f a, Unifiable f a) => f -> a -> Cond db r
    eq = (==.)

-- | Converts value to 'Expr'. It can help to pass values of different types into functions which expect arguments of the same type, like (+).
liftExpr :: ExpressionOf db r a a' => a -> Expr db r a'
liftExpr a = Expr $ toExpr a

{-# DEPRECATED toArith "Please use liftExpr instead" #-}

-- | It is kept for compatibility with older Groundhog versions and can be replaced with "liftExpr".
toArith :: ExpressionOf db r a a' => a -> Expr db r a'
toArith = liftExpr
