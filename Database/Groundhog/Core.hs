{-# LANGUAGE GADTs, TypeFamilies, ExistentialQuantification, StandaloneDeriving, TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, OverlappingInstances, ScopedTypeVariables, GeneralizedNewtypeDeriving, UndecidableInstances, EmptyDataDecls #-}

-- | This module defines the functions and datatypes used throughout the framework.
-- Most of them are for internal use
module Database.Groundhog.Core
  ( 
  -- * Main types
    PersistEntity(..)
  , PersistValue(..)
  , PersistField(..)
  , SinglePersistField(..)
  , PurePersistField(..)
  , PrimitivePersistField(..)
  , Embedded(..)
  , Selector(..)
  , Key(..)
  -- * Constructing expressions
  -- $exprDoc
  , Cond(..)
  , ExprRelation(..)
  , Update(..)
  , (=.), (&&.), (||.), (==.), (/=.), (<.), (<=.), (>.), (>=.), (~>)
  , wrapPrim
  , toArith
  , FieldLike(..)
  , Expression(..)
  , Numeric
  , Arith(..)
  , Expr(..)
  , Order(..)
  -- * Type description
  , DbType(..)
  , NamedType
  , namedType
  , getName
  , getType
  , EntityDef(..)
  , ConstructorDef(..)
  , Constructor(..)
  , Constraint
  -- * Migration
  , SingleMigration
  , NamedMigrations
  , Migration
  -- * Database
  , PersistBackend(..)
  , RowPopper
  , DbPersist(..)
  , runDbPersist
  -- * Other
  , failMessage
  ) where

import Control.Applicative(Applicative)
import Control.Monad.Trans.Class(MonadTrans(..))
import Control.Monad.IO.Class(MonadIO(..))
import Control.Monad.IO.Control (MonadControlIO)
import Control.Monad.Trans.Reader(ReaderT, runReaderT)
import Control.Monad.Trans.State(StateT)
import Data.Bits(bitSize)
import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Map(Map)
import Data.Time(Day, TimeOfDay, UTCTime)
import Unsafe.Coerce(unsafeCoerce)

-- | Only instances of this class can be persisted in a database
class PersistField v => PersistEntity v where
  -- | This type is used for typesafe manipulation of separate fields of datatype v.
  -- Each constructor in 'Field' corresponds to its field in a datatype v.
  -- It is parametrised by constructor phantom type and field value type.
  data Field v :: * -> * -> *
  -- | Returns a complete description of the type
  entityDef :: v -> EntityDef
  -- | Marshalls value to a list of 'PersistValue' ready for insert to a database
  toEntityPersistValues   :: PersistBackend m => v -> m [PersistValue]
  -- | Constructs the value from the list of 'PersistValue'
  fromEntityPersistValues :: PersistBackend m => [PersistValue] -> m v
  -- | Returns constructor number and a list of constraint names and corresponding field names with their values
  getConstraints    :: v -> (Int, [(String, [(String, PersistValue)])])
  -- | Is internally used by FieldLike Field instance
  -- We could avoid this function if class FieldLike allowed FieldLike Fields Data or FieldLike (Fields Data). However that would require additional extensions in user-space code
  entityFieldChain :: Field v c a -> Either String [(String, NamedType)]

-- | A unique identifier of a value stored in a database
data PersistEntity v => Key v = Key Int64 deriving (Show, Eq)

data Any

type family MoreSpecific a b
type instance MoreSpecific Any a = a
type instance MoreSpecific a Any = a
type instance MoreSpecific a a = a
type instance MoreSpecific Any Any = Any

class TypesCastV x y z | x y -> z
-- instance TypesCastV x x x would not work. For example, it does not match TypesCastV (Type a1) (Type a2) z
instance (x ~ y, MoreSpecific x y ~ z) => TypesCastV x y z
instance TypesCastV Any x x
instance TypesCastV x Any x
instance TypesCastV Any Any Any

class TypesEqualC x y
instance TypesEqualC x x
instance TypesEqualC Any x
instance TypesEqualC x Any
instance TypesEqualC Any Any

class TypesCastC x y z | x y -> z
instance (TypesEqualC x y, MoreSpecific x y ~ z) => TypesCastC x y z

class (Expression a, Expression b) => TypeCast a b v c | a b -> v, a b -> c
instance (Expression a, Expression b, TypesCastV (FuncV a) (FuncV b) v, TypesCastC (FuncC a) (FuncC b) c) => TypeCast a b v c
--instance (Expression a, Expression b, FuncV a ~ Any, FuncC a ~ Any, FuncV b ~ v, FuncC b ~ c) => TypeCast a b v c
--instance (FuncV a ~ v, FuncC a ~ c) => TypeCast a Any v c
--instance TypeCast Any Any Any Any

-- $exprDoc
-- The expressions are used in conditions and right part of Update statement.
-- Despite the wordy types of the comparison functions, they are simple to use.
-- Type of the compared polymorphic values like numbers or Nothing must be supplied manually. Example:
--
-- @
-- StringField ==. \"abc\" &&. NumberField >. (0 :: Int) ||. MaybeField ==. (Nothing :: Maybe String) ||. MaybeField ==. Just \"def\"
-- @
--

-- | Represents condition for a query.
data Cond v c =
    And (Cond v c) (Cond v c)
  | Or  (Cond v c) (Cond v c)
  | Not (Cond v c)
  | forall a.PersistField a => Compare ExprRelation (Expr v c a) (Expr v c a)
  -- | Lookup will be performed only in table for the specified constructor c. To fetch value by key without constructor limitation use 'get'
  | KeyIs (Key v)

data ExprRelation = Eq | Ne | Gt | Lt | Ge | Le deriving Show

data Update v c = forall a f . FieldLike f => Update (f v c a) (Expr v c a)
--deriving instance (Show (Field c a)) => Show (Update c)

-- | Defines sort order of a result-set
data Order v c = forall a f . FieldLike f => Asc  (f v c a)
               | forall a f . FieldLike f => Desc (f v c a)

-- | Generalises data that can occur in expressions (so far there are regular Field and SubField).
class FieldLike f where
  -- | It is used to map field to column names. It can be either a column name for a regular field of non-embedded type or a list of this field and the outer fields in reverse order. Eg, fieldChain $ SomeField ~> Tuple2_0Selector may result in Right [("val0", DbString), ("some", DbEmbedded False [namedType "", namedType True])].
  -- Function fieldChain can be simplified to f v c a -> [(String, NamedType)]. Datatype Either is used for optimisation of the common case, eg Field v c Int.
  fieldChain :: PersistEntity v => f v c a -> Either String [(String, NamedType)]

class Embedded v where
  data Selector v :: * -> *
  selectorNum :: Selector v a -> Int

infixl 5 ~>
(~>) :: (FieldLike f, PersistEntity v, Embedded a) => f v c a -> Selector a a' -> SubField v c a'
field ~> sel = case fieldChain field of
  Right (fs@((_, f):_)) -> case getType f of
    DbEmbedded _ ts -> SubField (ts !! selectorNum sel : fs)
    other -> error $ "(~>): cannot get subfield of non-embedded type " ++ show other
  other -> error $ "(~>): cannot get subfield of " ++ show other

newtype SubField v c a = SubField [(String, NamedType)]

instance FieldLike SubField where
  fieldChain (SubField fs) = Right fs

instance FieldLike Field where
  fieldChain = entityFieldChain

-- TODO: UGLY: we use unsafeCoerce to cast phantom types Any and Any to more specific type if possible. The safety is assured by TypeEqual and TypeEqualC classes. I hope it will work w/o woes of segfaults

-- | Update field
infixr 3 =.
(=.) ::
  ( Expression a
  , TypesCastV v (FuncV a) v
  , TypesCastC c (FuncC a) c
  , FieldLike f)
  => f v c (FuncA a) -> a -> Update v c
f =. a = Update f (unsafeCoerceExpr $ wrap a)

-- | Boolean \"and\" operator.
(&&.) :: (TypesCastV v1 v2 v3, TypesCastC c1 c2 c3) =>
  Cond v1 c1 -> Cond v2 c2 -> Cond v3 c3

-- | Boolean \"or\" operator.  
(||.) :: (TypesCastV v1 v2 v3, TypesCastC c1 c2 c3) =>
  Cond v1 c1 -> Cond v2 c2 -> Cond v3 c3

infixr 3 &&.
a &&. b = And (unsafeCoerce a) (unsafeCoerce b)

infixr 2 ||.
a ||. b = Or (unsafeCoerce a) (unsafeCoerce b)

unsafeCoerceExpr :: Expr v1 c1 a -> Expr v2 c2 a
unsafeCoerceExpr = unsafeCoerce

(==.), (/=.) ::
  ( TypeCast a b v c
  , FuncA a ~ FuncA b
  , PersistField (FuncA a))
  => a -> b -> Cond v c

(<.), (<=.), (>.), (>=.) ::
  ( TypeCast a b v c
  , FuncA a ~ FuncA b
  , PersistField (FuncA a))
  => a -> b -> Cond v c

infix 4 ==., <., <=., >., >=.
a ==. b = Compare Eq (unsafeCoerceExpr $ wrap a) (unsafeCoerceExpr $ wrap b)
a /=. b = Compare Ne (unsafeCoerceExpr $ wrap a) (unsafeCoerceExpr $ wrap b)
a <.  b = Compare Lt (unsafeCoerceExpr $ wrap a) (unsafeCoerceExpr $ wrap b)
a <=. b = Compare Le (unsafeCoerceExpr $ wrap a) (unsafeCoerceExpr $ wrap b)
a >.  b = Compare Gt (unsafeCoerceExpr $ wrap a) (unsafeCoerceExpr $ wrap b)
a >=. b = Compare Ge (unsafeCoerceExpr $ wrap a) (unsafeCoerceExpr $ wrap b)

newtype Monad m => DbPersist conn m a = DbPersist { unDbPersist :: ReaderT conn m a }
  deriving (Monad, MonadIO, Functor, Applicative, MonadControlIO, MonadTrans)

runDbPersist :: Monad m => DbPersist conn m a -> conn -> m a
runDbPersist = runReaderT.unDbPersist

class Monad m => PersistBackend m where
  -- | Insert a new record to a database and return its 'Key'
  insert        :: PersistEntity v => v -> m (Key v)
  -- | Try to insert a record and return Right newkey. If there is a constraint violation, Left oldkey is returned
  -- , where oldkey is an identifier of the record with the same constraint values. Note that if several constraints are violated, a key of an arbitrary matching record is returned.
  insertBy      :: PersistEntity v => v -> m (Either (Key v) (Key v))
  -- | Replace a record with the given key. Result is undefined if the record does not exist.
  replace       :: PersistEntity v => Key v -> v -> m ()
  -- | Return a list of the records satisfying the condition
  select        :: (PersistEntity v, Constructor c)
                => Cond v c
                -> [Order v c]
                -> Int -- ^ limit
                -> Int -- ^ offset
                -> m [(Key v, v)]
  -- | Return a list of all records. Order is undefined
  selectAll     :: PersistEntity v => m [(Key v, v)]
  -- | Fetch an entity from a database
  get           :: PersistEntity v => Key v -> m (Maybe v)
  -- | Update the records satisfying the condition
  update        :: (PersistEntity v, Constructor c) => [Update v c] -> Cond v c -> m ()
  -- | Remove the records satisfying the condition
  delete        :: (PersistEntity v, Constructor c) => Cond v c -> m ()
  -- | Remove the record with given key. No-op if the record does not exist
  deleteByKey   :: PersistEntity v => Key v -> m ()
  -- | Count total number of records satisfying the condition
  count         :: (PersistEntity v, Constructor c) => Cond v c -> m Int
  -- | Count total number of records with all constructors
  countAll      :: PersistEntity v => v -> m Int
  -- | Check database schema and create migrations for the entity and the entities it contains
  migrate       :: PersistEntity v => v -> Migration m
  -- | Execute raw query
  executeRaw    :: Bool           -- ^ keep in cache
                -> String         -- ^ query
                -> [PersistValue] -- ^ positional parameters
                -> m ()
  -- | Execute raw query with results
  queryRaw      :: Bool           -- ^ keep in cache
                -> String         -- ^ query
                -> [PersistValue] -- ^ positional parameters
                -> (RowPopper m -> m a) -- ^ results processing function
                -> m a
  insertList    :: PersistField a => [a] -> m Int64
  getList       :: PersistField a => Int64 -> m [a]
  
type RowPopper m = m (Maybe [PersistValue])

type Migration m = StateT NamedMigrations m ()

-- | Datatype names and corresponding migrations
type NamedMigrations = Map String SingleMigration

-- | Either error messages or migration queries with safety flag and execution order
type SingleMigration = Either [String] [(Bool, Int, String)]

-- | Describes an ADT.
data EntityDef = EntityDef {
  -- | Emtity name
    entityName   :: String
  -- | Named types of the instantiated polymorphic type parameters
  , typeParams   :: [NamedType]
  -- | List of entity constructors definitions
  , constructors :: [ConstructorDef]
} deriving (Show, Eq)

-- | Describes an entity constructor
data ConstructorDef = ConstructorDef {
  -- | Number of the constructor in the ADT
    constrNum     :: Int
  -- | Constructor name
  , constrName    :: String
  -- | Parameter names with their named type
  , constrParams  :: [(String, NamedType)]
  -- | Uniqueness constraints on the constructor fiels
  , constrConstrs :: [Constraint]
} deriving (Show, Eq)

-- | Phantom constructors are made instances of this class. This class should be used only by Template Haskell codegen
class Constructor a where
  -- returning ConstructorDef seems more logical, but it would require the value datatype
  -- it can be supplied either as a part of constructor type, eg instance Constructor (MyDataConstructor (MyData a)) which requires -XFlexibleInstances
  -- or as a separate type, eg instance Constructor MyDataConstructor (MyData a) which requires -XMultiParamTypeClasses
  -- the phantoms are primarily used to get the constructor name. So to keep user code cleaner we return only the name and number, which can be later used to get ConstructorDef from the EntityDef
  phantomConstrName :: a -> String
  phantomConstrNum :: a -> Int

-- | Constraint name and list of the field names that form a unique combination.
-- Only fields of 'PrimitivePersistField' types can be used in a constraint
type Constraint = (String, [String])

-- | A DB data type. Naming attempts to reflect the underlying Haskell
-- datatypes, eg DbString instead of DbVarchar. Different databases may
-- have different translations for these types.
data DbType = DbString
            | DbInt32
            | DbInt64
            | DbReal
            | DbBool
            | DbDay
            | DbTime
            | DbDayTime
            | DbBlob    -- ByteString
-- More complex types
            | DbMaybe NamedType
            | DbList NamedType
            -- | The first argument is a flag which defines if the field names should be concatenated with the outer field name (False) or used as is which provides full control over table column names (True). False should be the default value so that a datatype can be embedded without name conflict concern. The second argument list of field names and field types.
            | DbEmbedded Bool [(String, NamedType)]
            | DbEntity EntityDef
  deriving (Eq, Show)

-- TODO: this type can be changed to avoid storing the value itself. For example, ([String, DbType). Restriction: can be used to get DbType and name
-- | It is used to store type 'DbType' and persist name of a value
data NamedType = forall v . PersistField v => NamedType v

namedType :: PersistField v => v -> NamedType
namedType = NamedType

getName :: NamedType -> String
getName (NamedType v) = persistName v

getType :: NamedType -> DbType
getType (NamedType v) = dbType v

instance Show NamedType where
  show (NamedType v) = show (dbType v)

-- rely on the invariant that no two types have the same name
instance Eq NamedType where
  (NamedType v1) == (NamedType v2) = persistName v1 == persistName v2

-- | A raw value which can be stored in any backend and can be marshalled to
-- and from a 'PersistField'.
data PersistValue = PersistString String
                  | PersistByteString ByteString
                  | PersistInt64 Int64
                  | PersistDouble Double
                  | PersistBool Bool
                  | PersistDay Day
                  | PersistTimeOfDay TimeOfDay
                  | PersistUTCTime UTCTime
                  | PersistNull
  deriving (Show, Eq)

-- | Arithmetic expressions which can include fields and literals
data Arith v c a =
    Plus  (Arith v c a) (Arith v c a)
  | Minus (Arith v c a) (Arith v c a)
  | Mult  (Arith v c a) (Arith v c a)
  | Abs   (Arith v c a)
  | forall f . FieldLike f => ArithField (f v c a)
  | Lit   Int64

instance PersistEntity v => Eq (Arith v c a) where
  (Plus a1 b1)   == (Plus a2 b2)   = a1 == a2 && b1 == b2
  (Minus a1 b1)  == (Minus a2 b2)  = a1 == a2 && b1 == b2
  (Mult a1 b1)   == (Mult a2 b2)   = a1 == a2 && b1 == b2
  (Abs a)        == (Abs b)        = a == b
  (ArithField a) == (ArithField b) = fieldChain a == fieldChain b
  (Lit a)        == (Lit b)        = a == b
  _              == _              = False

instance PersistEntity v => Show (Arith v c a) where
  show (Plus a b)     = "Plus (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Minus a b)    = "Minus (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Mult a b)     = "Mult (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Abs a)        = "Abs (" ++ show a ++ ")"
  show (ArithField a) = "ArithField " ++ show (fieldChain a)
  show (Lit a)        = "Lit " ++ show a

instance (PersistEntity v, Numeric a) => Num (Arith v c a) where
  a + b       = Plus  a b
  a - b       = Minus a b
  a * b       = Mult  a b
  abs         = Abs
  signum      = error "no signum"
  fromInteger = Lit . fromInteger
  
-- | Convert field to an arithmetic value
toArith :: (FieldLike f, PersistEntity v) => f v c a -> Arith v c a
toArith = ArithField

-- | Constraint for use in arithmetic expressions. 'Num' is not used to explicitly include only types supported by the library .
-- TODO: consider replacement with 'Num'
class Numeric a

-- | Types which when converted to 'PersistValue' are never NULL.
-- Consider the type @Maybe (Maybe a)@. Now Nothing is stored as NULL, so we cannot distinguish between Just Nothing and Nothing which is a problem.
-- The purpose of this class is to ban the inner Maybe's.
-- Maybe this class can be removed when support for inner Maybe's appears.
class NeverNull a

-- | Datatypes which can be converted directly to 'PersistValue'
class (SinglePersistField a, PurePersistField a) => PrimitivePersistField a where
  toPrim :: a -> PersistValue
  fromPrim :: PersistValue -> a

-- | Used to uniformly represent fields, literals and arithmetic expressions.
-- A value should be converted to 'Expr' for usage in expressions
data Expr v c a where
  ExprField :: (FieldLike f, PersistEntity v) => f v c a -> Expr v c a
  ExprArith :: PersistEntity v => Arith v c a -> Expr v c a
  ExprPure :: forall a v c a' . PurePersistField a => a -> Expr v c a'

-- I wish wrap could return Expr with both fixed and polymorphic v&c. Any is used to emulate polymorphic types.
-- | Instances of this type can be converted to 'Expr'
class Expression a where
  type FuncV a; type FuncC a; type FuncA a
  wrap :: a -> Expr (FuncV a) (FuncC a) (FuncA a)

-- | By default during converting values of certain types to 'Expr', the types can be changed. For example, @'Key' a@ is transformed into @a@.
-- It is convenient because the fields usually contain reference to a certain datatype, not its 'Key'.
-- But sometimes when automatic transformation gets in the way function 'wrapPrim' will help. Use it when a field in a datatype has type @(Key a)@ or @Maybe (Key a)@. Example:
--
-- @
--data Example = Example {entity1 :: Maybe Smth, entity2 :: Key Smth}
--Entity1Field ==. Just k &&. Entity2Field ==. wrapPrim k
-- @
wrapPrim :: PrimitivePersistField a => a -> Expr Any Any a
-- We cannot create different Expression instances for (Field v c a) and (Field v c (Key a))
-- so that Func (Field v c a) = a and Func (Field v c (Key a)) = a
-- because of the type families overlap restrictions. Neither we can create different instances for Key a
wrapPrim = ExprPure

-- | Represents everything which can be put into a database. This data can be stored in multiple columns and tables. To get value of those columns we might need to access another table. That is why the result type is monadic.
class PersistField a where
  -- | Return name of the type. If it is polymorhic, the names of parameter types are separated with \"$\" symbol
  persistName :: a -> String
  -- | Convert a value into something which can be stored in a database column.
  -- Note that for complex datatypes it may insert them to return identifier
  toPersistValues :: PersistBackend m => a -> m ([PersistValue] -> [PersistValue])
  -- | Constructs a value from a 'PersistValue'. For complex datatypes it may query the database
  fromPersistValues :: PersistBackend m => [PersistValue] -> m (a, [PersistValue])
  -- | Description of value type
  dbType :: a -> DbType

-- | Represents all datatypes that map into a single column. Getting value for that column might require monadic actions to access other tables.
class PersistField a => SinglePersistField a where
  toSinglePersistValue :: PersistBackend m => a -> m PersistValue
  fromSinglePersistValue :: PersistBackend m => PersistValue -> m a

-- | Represents all datatypes that map into several columns. Getting values for those columns is pure.
class PersistField a => PurePersistField a where
  toPurePersistValues :: a -> ([PersistValue] -> [PersistValue])
  fromPurePersistValues :: [PersistValue] -> (a, [PersistValue])

---- INSTANCES

instance Embedded (a, b) where
  data Selector (a, b) constr where
    Tuple2_0Selector :: Selector (a, b) a
    Tuple2_1Selector :: Selector (a, b) b
  selectorNum Tuple2_0Selector = 0
  selectorNum Tuple2_1Selector = 1

instance Embedded (a, b, c) where
  data Selector (a, b, c) constr where
    Tuple3_0Selector :: Selector (a, b, c) a
    Tuple3_1Selector :: Selector (a, b, c) b
    Tuple3_2Selector :: Selector (a, b, c) c
  selectorNum Tuple3_0Selector = 0
  selectorNum Tuple3_1Selector = 1
  selectorNum Tuple3_2Selector = 2

instance Embedded (a, b, c, d) where
  data Selector (a, b, c, d) constr where
    Tuple4_0Selector :: Selector (a, b, c, d) a
    Tuple4_1Selector :: Selector (a, b, c, d) b
    Tuple4_2Selector :: Selector (a, b, c, d) c
    Tuple4_3Selector :: Selector (a, b, c, d) d
  selectorNum Tuple4_0Selector = 0
  selectorNum Tuple4_1Selector = 1
  selectorNum Tuple4_2Selector = 2
  selectorNum Tuple4_3Selector = 3

instance Embedded (a, b, c, d, e) where
  data Selector (a, b, c, d, e) constr where
    Tuple5_0Selector :: Selector (a, b, c, d, e) a
    Tuple5_1Selector :: Selector (a, b, c, d, e) b
    Tuple5_2Selector :: Selector (a, b, c, d, e) c
    Tuple5_3Selector :: Selector (a, b, c, d, e) d
    Tuple5_4Selector :: Selector (a, b, c, d, e) e
  selectorNum Tuple5_0Selector = 0
  selectorNum Tuple5_1Selector = 1
  selectorNum Tuple5_2Selector = 2
  selectorNum Tuple5_3Selector = 3
  selectorNum Tuple5_4Selector = 4

instance PrimitivePersistField a => SinglePersistField a where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance PrimitivePersistField a => PurePersistField a where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField () where
  toPurePersistValues _ = id
  fromPurePersistValues xs = ((), xs)

instance (PurePersistField a, PurePersistField b) => PurePersistField (a, b) where
  toPurePersistValues (a, b) = toPurePersistValues a . toPurePersistValues b
  fromPurePersistValues xs = let
    (a, rest0) = fromPurePersistValues xs
    (b, rest1) = fromPurePersistValues rest0
    in ((a, b), rest1)

instance (PurePersistField a, PurePersistField b, PurePersistField c) => PurePersistField (a, b, c) where
  toPurePersistValues (a, b, c) = toPurePersistValues a . toPurePersistValues b . toPurePersistValues c
  fromPurePersistValues xs = let
    (a, rest0) = fromPurePersistValues xs
    (b, rest1) = fromPurePersistValues rest0
    (c, rest2) = fromPurePersistValues rest1
    in ((a, b, c), rest2)
  
instance (PurePersistField a, PurePersistField b, PurePersistField c, PurePersistField d) => PurePersistField (a, b, c, d) where
  toPurePersistValues (a, b, c, d) = toPurePersistValues a . toPurePersistValues b . toPurePersistValues c . toPurePersistValues d
  fromPurePersistValues xs = let
    (a, rest0) = fromPurePersistValues xs
    (b, rest1) = fromPurePersistValues rest0
    (c, rest2) = fromPurePersistValues rest1
    (d, rest3) = fromPurePersistValues rest2
    in ((a, b, c, d), rest3)
  
instance (PurePersistField a, PurePersistField b, PurePersistField c, PurePersistField d, PurePersistField e) => PurePersistField (a, b, c, d, e) where
  toPurePersistValues (a, b, c, d, e) = toPurePersistValues a . toPurePersistValues b . toPurePersistValues c . toPurePersistValues d . toPurePersistValues e
  fromPurePersistValues xs = let
    (a, rest0) = fromPurePersistValues xs
    (b, rest1) = fromPurePersistValues rest0
    (c, rest2) = fromPurePersistValues rest1
    (d, rest3) = fromPurePersistValues rest2
    (e, rest4) = fromPurePersistValues rest3
    in ((a, b, c, d, e), rest4)

instance Numeric Int
instance Numeric Int8
instance Numeric Int16
instance Numeric Int32
instance Numeric Int64
instance Numeric Word8
instance Numeric Word16
instance Numeric Word32
instance Numeric Word64
instance Numeric Double

instance PrimitivePersistField String where
  toPrim = PersistString
  fromPrim (PersistString s) = s
  fromPrim (PersistByteString bs) = T.unpack $ T.decodeUtf8With T.lenientDecode bs
  fromPrim (PersistInt64 i) = show i
  fromPrim (PersistDouble d) = show d
  fromPrim (PersistDay d) = show d
  fromPrim (PersistTimeOfDay d) = show d
  fromPrim (PersistUTCTime d) = show d
  fromPrim (PersistBool b) = show b
  fromPrim PersistNull = error "Unexpected null"

instance PrimitivePersistField T.Text where
  toPrim = PersistString . T.unpack
  fromPrim (PersistByteString bs) = T.decodeUtf8With T.lenientDecode bs
  fromPrim x = T.pack $ fromPrim x

instance PrimitivePersistField ByteString where
  toPrim = PersistByteString
  fromPrim (PersistByteString a) = a
  fromPrim x = T.encodeUtf8 . T.pack $ fromPrim x

instance PrimitivePersistField Int where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Int8 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Int16 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Int32 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Int64 where
  toPrim = PersistInt64
  fromPrim (PersistInt64 a) = a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Word8 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Word16 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Word32 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Word64 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Double where
  toPrim = PersistDouble
  fromPrim (PersistDouble a) = a
  fromPrim x = error $ "Expected Double, received: " ++ show x

instance PrimitivePersistField Bool where
  toPrim = PersistBool
  fromPrim (PersistBool a) = a
  fromPrim (PersistInt64 i) = i /= 0
  fromPrim x = error $ "Expected Bool, received: " ++ show x

instance PrimitivePersistField Day where
  toPrim = PersistDay
  fromPrim (PersistDay a) = a
  fromPrim x = readHelper x ("Expected Day, received: " ++ show x)

instance PrimitivePersistField TimeOfDay where
  toPrim = PersistTimeOfDay
  fromPrim (PersistTimeOfDay a) = a
  fromPrim x = readHelper x ("Expected TimeOfDay, received: " ++ show x)

instance PrimitivePersistField UTCTime where
  toPrim = PersistUTCTime
  fromPrim (PersistUTCTime a) = a
  fromPrim x = readHelper x ("Expected UTCTime, received: " ++ show x)

instance PersistEntity a => PrimitivePersistField (Key a) where
  toPrim (Key a) = PersistInt64 a
  fromPrim (PersistInt64 a) = Key a
  fromPrim x = error $ "Expected Integer(entity key), received: " ++ show x

instance (PrimitivePersistField a, NeverNull a) => PrimitivePersistField (Maybe a) where
  toPrim = maybe PersistNull toPrim
  fromPrim PersistNull = Nothing
  fromPrim x = Just $ fromPrim x

instance NeverNull String
instance NeverNull T.Text
instance NeverNull ByteString
instance NeverNull Int
instance NeverNull Int64
instance NeverNull Double
instance NeverNull Bool
instance NeverNull Day
instance NeverNull TimeOfDay
instance NeverNull UTCTime
instance NeverNull (Key a)
instance PersistEntity a => NeverNull a

instance Expression (Expr v c a) where
  type FuncV (Expr v c a) = v
  type FuncC (Expr v c a) = c
  type FuncA (Expr v c a) = a
  wrap = id

instance PersistEntity v => Expression (Field v c a) where
  type FuncV (Field v c a) = v
  type FuncC (Field v c a) = c
  type FuncA (Field v c a) = a
  wrap = ExprField

instance PersistEntity v => Expression (SubField v c a) where
  type FuncV (SubField v c a) = v
  type FuncC (SubField v c a) = c
  type FuncA (SubField v c a) = a
  wrap = ExprField

instance PersistEntity v => Expression (Arith v c a) where
  type FuncV (Arith v c a) = v
  type FuncC (Arith v c a) = c
  type FuncA (Arith v c a) = a
  wrap = ExprArith

instance (Expression a, PrimitivePersistField a, NeverNull a) => Expression (Maybe a) where
  type FuncV (Maybe a) = Any
  type FuncC (Maybe a) = Any
  type FuncA (Maybe a) = (Maybe (FuncA a))
  wrap = ExprPure

instance PersistEntity a => Expression (Key a) where
  type FuncV (Key a) = Any; type FuncC (Key a) = Any; type FuncA (Key a) = a
  wrap = ExprPure

instance Expression () where
  type FuncV () = Any
  type FuncC () = Any
  type FuncA () = ()
  wrap = ExprPure

instance PurePersistField (a, b) => Expression (a, b) where
  type FuncV (a, b) = Any
  type FuncC (a, b) = Any
  type FuncA (a, b) = (a, b)
  wrap = ExprPure

instance PurePersistField (a, b, c) => Expression (a, b, c) where
  type FuncV (a, b, c) = Any
  type FuncC (a, b, c) = Any
  type FuncA (a, b, c) = (a, b, c)
  wrap = ExprPure

instance PurePersistField (a, b, c, d) => Expression (a, b, c, d) where
  type FuncV (a, b, c, d) = Any
  type FuncC (a, b, c, d) = Any
  type FuncA (a, b, c, d) = (a, b, c, d)
  wrap = ExprPure

instance PurePersistField (a, b, c, d, e) => Expression (a, b, c, d, e) where
  type FuncV (a, b, c, d, e) = Any
  type FuncC (a, b, c, d, e) = Any
  type FuncA (a, b, c, d, e) = (a, b, c, d, e)
  wrap = ExprPure

instance Expression Int where
  type FuncV Int = Any; type FuncC Int = Any; type FuncA Int = Int
  wrap = ExprPure

instance Expression Int8 where
  type FuncV Int8 = Any; type FuncC Int8 = Any; type FuncA Int8 = Int8
  wrap = ExprPure

instance Expression Int16 where
  type FuncV Int16 = Any; type FuncC Int16 = Any; type FuncA Int16 = Int16
  wrap = ExprPure

instance Expression Int32 where
  type FuncV Int32 = Any; type FuncC Int32 = Any; type FuncA Int32 = Int32
  wrap = ExprPure

instance Expression Int64 where
  type FuncV Int64 = Any; type FuncC Int64 = Any; type FuncA Int64 = Int64
  wrap = ExprPure

instance Expression Word8 where
  type FuncV Word8 = Any; type FuncC Word8 = Any; type FuncA Word8 = Word8
  wrap = ExprPure

instance Expression Word16 where
  type FuncV Word16 = Any; type FuncC Word16 = Any; type FuncA Word16 = Word16
  wrap = ExprPure

instance Expression Word32 where
  type FuncV Word32 = Any; type FuncC Word32 = Any; type FuncA Word32 = Word32
  wrap = ExprPure

instance Expression Word64 where
  type FuncV Word64 = Any; type FuncC Word64 = Any; type FuncA Word64 = Word64
  wrap = ExprPure

instance Expression String where
  type FuncV String = Any; type FuncC String = Any; type FuncA String = String
  wrap = ExprPure

instance Expression ByteString where
  type FuncV ByteString = Any; type FuncC ByteString = Any; type FuncA ByteString = ByteString
  wrap = ExprPure

instance Expression T.Text where
  type FuncV T.Text = Any; type FuncC T.Text = Any; type FuncA T.Text = T.Text
  wrap = ExprPure

instance Expression Bool where
  type FuncV Bool = Any; type FuncC Bool = Any; type FuncA Bool = Bool
  wrap = ExprPure

readHelper :: Read a => PersistValue -> String -> a
readHelper s errMessage = case s of
  PersistString str -> readHelper' str
  PersistByteString str -> readHelper' (unpack str)
  _ -> error errMessage
  where
    readHelper' str = case reads str of
      (a, _):_ -> a
      _        -> error errMessage

failMessage :: PersistField a => a -> [PersistValue] -> String
failMessage a xs = "Invalid list for " ++ persistName a ++ ": " ++ show xs

primFromPersistValue :: (PersistBackend m, PrimitivePersistField a) => [PersistValue] -> m (a, [PersistValue])
primFromPersistValue (x:xs) = return (fromPrim x, xs)
primFromPersistValue xs = (\a -> fail (failMessage a xs) >> return (a, xs)) undefined

instance PersistField ByteString where
  persistName _ = "ByteString"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbBlob

instance PersistField String where
  persistName _ = "String"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbString

instance PersistField T.Text where
  persistName _ = "Text"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbString

instance PersistField Int where
  persistName _ = "Int"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType a = if bitSize a == 32 then DbInt32 else DbInt64

instance PersistField Int8 where
  persistName _ = "Int8"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Int16 where
  persistName _ = "Int16"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Int32 where
  persistName _ = "Int32"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Int64 where
  persistName _ = "Int64"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt64

instance PersistField Word8 where
  persistName _ = "Word8"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Word16 where
  persistName _ = "Word16"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Word32 where
  persistName _ = "Word32"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt64

instance PersistField Word64 where
  persistName _ = "Word64"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt64

instance PersistField Double where
  persistName _ = "Double"
  toPersistValues a = return (PersistDouble a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbReal

instance PersistField Bool where
  persistName _ = "Bool"
  toPersistValues a = return (PersistBool a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbBool

instance PersistField Day where
  persistName _ = "Day"
  toPersistValues a = return (PersistDay a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbDay

instance PersistField TimeOfDay where
  persistName _ = "TimeOfDay"
  toPersistValues a = return (PersistTimeOfDay a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbTime

instance PersistField UTCTime where
  persistName _ = "UTCTime"
  toPersistValues a = return (PersistUTCTime a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbDayTime

instance SinglePersistField a => PersistField (Maybe a) where
  persistName (_ :: Maybe a) = "Maybe$" ++ persistName (undefined :: a)
  toPersistValues Nothing = return (PersistNull:)
  toPersistValues (Just a) = toSinglePersistValue a >>= \x -> return (x:)
  fromPersistValues [] = fail "fromPersistValues Maybe: empty list"
  fromPersistValues (PersistNull:xs) = return (Nothing, xs)
  fromPersistValues (x:xs) = fromSinglePersistValue x >>= \x' -> return (Just x', xs)
  dbType (_ :: Maybe a) = DbMaybe $ namedType (undefined :: a)
  
instance (PersistEntity a) => PersistField (Key a) where
  persistName (_ :: Key a) = "Key$" ++ persistName (undefined :: a)
  toPersistValues (Key a) = return (PersistInt64 a:)
  fromPersistValues = primFromPersistValue
  dbType (_ :: Key a) = DbEntity $ entityDef (undefined :: a)

instance (PersistField a) => PersistField [a] where
  persistName (_ :: [a]) = "List$$" ++ persistName (undefined :: a)
  toPersistValues l = insertList l >>= toPersistValues
  fromPersistValues [] = fail "fromPersistValues []: empty list"
  fromPersistValues (x:xs) = getList (fromPrim x) >>= \l -> return (l, xs)
  dbType (_ :: [a]) = DbList $ namedType (undefined :: a)

instance PersistField () where
  persistName _ = "Unit$"
  toPersistValues _ = return id
  fromPersistValues xs = return ((), xs)
  dbType _ = DbEmbedded False []

tupleTypeHelper :: [NamedType] -> DbType
tupleTypeHelper ts = DbEmbedded False $ zip (map (\i -> "val" ++ show i) [0 :: Int ..]) ts

instance (PersistField a, PersistField b) => PersistField (a, b) where
  persistName (_ :: (a, b)) = "Tuple2$$" ++ persistName (undefined :: a) ++ "$" ++ persistName (undefined :: b)
  toPersistValues (a, b) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    return $ a' . b'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    return ((a, b), rest1)
  dbType (_ :: (a, b)) = tupleTypeHelper [namedType (undefined :: a), namedType (undefined :: b)]
  
instance (PersistField a, PersistField b, PersistField c) => PersistField (a, b, c) where
  persistName (_ :: (a, b, c)) = "Tuple3$$" ++ persistName (undefined :: a) ++ "$" ++ persistName (undefined :: b) ++ "$" ++ persistName (undefined :: c)
  toPersistValues (a, b, c) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    c' <- toPersistValues c
    return $ a' . b' . c'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    (c, rest2) <- fromPersistValues rest1
    return ((a, b, c), rest2)
  dbType (_ :: (a, b, c)) = tupleTypeHelper [namedType (undefined :: a), namedType (undefined :: b), namedType (undefined :: c)]
  
instance (PersistField a, PersistField b, PersistField c, PersistField d) => PersistField (a, b, c, d) where
  persistName (_ :: (a, b, c, d)) = "Tuple4$$" ++ persistName (undefined :: a) ++ "$" ++ persistName (undefined :: b) ++ "$" ++ persistName (undefined :: c) ++ "$" ++ persistName (undefined :: d)
  toPersistValues (a, b, c, d) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    c' <- toPersistValues c
    d' <- toPersistValues d
    return $ a' . b' . c' . d'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    (c, rest2) <- fromPersistValues rest1
    (d, rest3) <- fromPersistValues rest2
    return ((a, b, c, d), rest3)
  dbType (_ :: (a, b, c, d)) = tupleTypeHelper [namedType (undefined :: a), namedType (undefined :: b), namedType (undefined :: c), namedType (undefined :: d)]
  
instance (PersistField a, PersistField b, PersistField c, PersistField d, PersistField e) => PersistField (a, b, c, d, e) where
  persistName (_ :: (a, b, c, d, e)) = "Tuple5$$" ++ persistName (undefined :: a) ++ "$" ++ persistName (undefined :: b) ++ "$" ++ persistName (undefined :: c) ++ "$" ++ persistName (undefined :: d) ++ "$" ++ persistName (undefined :: e)
  toPersistValues (a, b, c, d, e) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    c' <- toPersistValues c
    d' <- toPersistValues d
    e' <- toPersistValues e
    return $ a' . b' . c' . d' . e'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    (c, rest2) <- fromPersistValues rest1
    (d, rest3) <- fromPersistValues rest2
    (e, rest4) <- fromPersistValues rest3
    return ((a, b, c, d, e), rest4)
  dbType (_ :: (a, b, c, d, e)) = tupleTypeHelper [namedType (undefined :: a), namedType (undefined :: b), namedType (undefined :: c), namedType (undefined :: d), namedType (undefined :: e)]
