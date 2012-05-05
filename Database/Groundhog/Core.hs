{-# LANGUAGE GADTs, TypeFamilies, ExistentialQuantification, MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving #-}
-- | This module defines the functions and datatypes used throughout the framework.
-- Most of them are for the internal use
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
  , Key(..)
  -- * Constructing expressions
  , Cond(..)
  , ExprRelation(..)
  , Update(..)
  , (~>)
  , toArith
  , FieldLike(..)
  , NeverNull
  , Numeric
  , Arith(..)
  , Expr(..)
  , SubField (..)
  , Order(..)
  -- * Type description
  , DbType(..)
  , EntityDef(..)
  , ConstructorDef(..)
  , Constructor(..)
  , Constraint(..)
  -- * Migration
  , SingleMigration
  , NamedMigrations
  , Migration
  -- * Database
  , PersistBackend(..)
  , RowPopper
  , DbPersist(..)
  , runDbPersist
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl (..), ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl (..))
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad (liftM)
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Time (Day, TimeOfDay, UTCTime)

-- | Only instances of this class can be persisted in a database
class SinglePersistField v => PersistEntity v where
  -- | This type is used for typesafe manipulation of separate fields of datatype v.
  -- Each constructor in 'Field' corresponds to its field in a datatype v.
  -- It is parametrised by constructor phantom type and field value type.
  data Field v :: * -> * -> *
  -- | Returns a complete description of the type
  entityDef :: v -> EntityDef
  -- | Marshalls value to a list of 'PersistValue' ready for insert to a database
  toEntityPersistValues :: PersistBackend m => v -> m [PersistValue]
  -- | Constructs the value from the list of 'PersistValue'
  fromEntityPersistValues :: PersistBackend m => [PersistValue] -> m v
  -- | Returns constructor number and a list of constraint names and corresponding field values
  getConstraints :: v -> (Int, [(String, [PersistValue])])
  -- | Is internally used by FieldLike Field instance
  -- We could avoid this function if class FieldLike allowed FieldLike Fields Data or FieldLike (Fields Data). However that would require additional extensions in user-space code
  entityFieldChain :: Field v c a -> Either String [(String, DbType)]

-- | A unique identifier of a value stored in a database
data PersistEntity v => Key v = Key Int64 deriving (Show, Eq)

-- | Represents condition for a query.
data Cond v c =
    And (Cond v c) (Cond v c)
  | Or  (Cond v c) (Cond v c)
  | Not (Cond v c)
  | forall a b . Compare ExprRelation (Expr v c a) (Expr v c b)
  -- | Lookup will be performed only in table for the specified constructor c. To fetch value by key without constructor limitation use 'get'
  | KeyIs (Key v)

data ExprRelation = Eq | Ne | Gt | Lt | Ge | Le deriving Show

data Update v c = forall f a b . FieldLike f => Update (f v c a) (Expr v c b)
--deriving instance (Show (Field c a)) => Show (Update c)

-- | Defines sort order of a result-set
data Order v c = forall a f . FieldLike f => Asc  (f v c a)
               | forall a f . FieldLike f => Desc (f v c a)

-- | Generalises data that can occur in expressions (so far there are regular Field and SubField).
class FieldLike f where
  -- | It is used to map field to column names. It can be either a column name for a regular field of non-embedded type or a list of this field and the outer fields in reverse order. Eg, fieldChain $ SomeField ~> Tuple2_0Selector may result in Right [(\"val0\", DbString), (\"some\", DbEmbedded False [dbType \"\", dbType True])].
  -- Function fieldChain can be simplified to f v c a -> [(String, DbType)]. Datatype Either is used for optimisation of the common case, eg Field v c Int.
  fieldChain :: PersistEntity v => f v c a -> Either String [(String, DbType)]

class PersistField v => Embedded v where
  data Selector v :: * -> *
  selectorNum :: Selector v a -> Int

infixl 5 ~>
(~>) :: (FieldLike f, PersistEntity v, Embedded a) => f v c a -> Selector a a' -> SubField v c a'
field ~> sel = case fieldChain field of
  Right (fs@((_, f):_)) -> case f of
    DbEmbedded _ ts -> SubField (ts !! selectorNum sel : fs)
    other -> error $ "(~>): cannot get subfield of non-embedded type " ++ show other
  other -> error $ "(~>): cannot get subfield of " ++ show other

newtype SubField v c a = SubField [(String, DbType)]

instance FieldLike SubField where
  fieldChain (SubField fs) = Right fs

instance FieldLike Field where
  fieldChain = entityFieldChain

newtype Monad m => DbPersist conn m a = DbPersist { unDbPersist :: ReaderT conn m a }
  deriving (Monad, MonadIO, Functor, Applicative, MonadTrans)

instance MonadBase IO m => MonadBase IO (DbPersist conn m) where
  liftBase = lift . liftBase

instance MonadTransControl (DbPersist conn) where
  newtype StT (DbPersist conn) a = StReader {unStReader :: a}
  liftWith f = DbPersist $ ReaderT $ \r -> f $ \t -> liftM StReader $ runReaderT (unDbPersist t) r
  restoreT = DbPersist . ReaderT . const . liftM unStReader

instance MonadBaseControl IO m => MonadBaseControl IO (DbPersist conn m) where
  newtype StM (DbPersist conn m) a = StMSP {unStMSP :: ComposeSt (DbPersist conn) m a}
  liftBaseWith = defaultLiftBaseWith StMSP
  restoreM     = defaultRestoreM   unStMSP

runDbPersist :: Monad m => DbPersist conn m a -> conn -> m a
runDbPersist = runReaderT . unDbPersist

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
  -- | Entity name. @entityName (entityDef v) == persistName v@
    entityName   :: String
  -- | Named types of the instantiated polymorphic type parameters
  , typeParams   :: [DbType]
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
  , constrParams  :: [(String, DbType)]
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
data Constraint = Constraint String [String] deriving (Show, Eq)

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
            | DbMaybe DbType
            | DbList String DbType -- list name and type of its argument
            -- | The first argument is a flag which defines if the field names should be concatenated with the outer field name (False) or used as is which provides full control over table column names (True). False should be the default value so that a datatype can be embedded without name conflict concern. The second argument list of field names and field types.
            | DbEmbedded Bool [(String, DbType)]
            | DbEntity EntityDef
  deriving (Eq, Show)

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
  ExprField :: (FieldLike f, PersistEntity v) => f v c a -> Expr v c (f v c a)
  ExprArith :: PersistEntity v => Arith v c a -> Expr v c (Arith v c a)
  ExprPure :: forall a v c . PurePersistField a => a -> Expr v c a

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
