{-# LANGUAGE GADTs, TypeFamilies, ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, EmptyDataDecls #-}
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
  , Projection(..)
  , RestrictionHolder
  , Unique
  , KeyForBackend(..)
  , BackendSpecific
  , ConstructorMarker
  , UniqueMarker
  , Proxy
  , HFalse
  , HTrue
  , ZT (..) -- ZonedTime wrapper
  , delim
  -- * Constructing expressions
  , Cond(..)
  , ExprRelation(..)
  , Update(..)
  , (~>)
  , toArith
  , FieldLike(..)
  , SubField (..)
  , AutoKeyField (..)
  , FieldChain
  , NeverNull
  , Numeric
  , Arith(..)
  , Expr(..)
  , Order(..)
  , HasSelectOptions(..)
  , SelectOptions(..)
  , limitTo
  , offsetBy
  , orderBy
  -- * Type description
  , DbType(..)
  , EntityDef(..)
  , EmbeddedDef(..)
  , ConstructorDef(..)
  , Constructor(..)
  , IsUniqueKey(..)
  , UniqueDef(..)
  -- * Migration
  , SingleMigration
  , NamedMigrations
  , Migration
  -- * Database
  , PersistBackend(..)
  , DbDescriptor(..)
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
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC, zonedTimeToLocalTime, zonedTimeZone)

-- | Only instances of this class can be persisted in a database
class (PersistField v, PurePersistField (AutoKey v)) => PersistEntity v where
  -- | This type is used for typesafe manipulation of separate fields of datatype v.
  -- Each constructor in 'Field' corresponds to its field in a datatype v.
  -- It is parametrised by constructor phantom type and field value type.
  data Field v :: ((* -> *) -> *) -> * -> *
  -- | A unique identifier of a value stored in a database. This may be a primary key, a constraint or unique indices. The second parameter is the key description.
  data Key v :: * -> *
  -- | This type is the default autoincremented key for the entity. If entity does not have such key, AutoKey v = ().
  type AutoKey v
  -- | This type is the default key for the entity.
  type DefaultKey v
  -- | Returns a complete description of the type
  entityDef :: v -> EntityDef
  -- | Marshalls value to a list of 'PersistValue' ready for insert to a database
  toEntityPersistValues :: PersistBackend m => v -> m ([PersistValue] -> [PersistValue])
  -- | Constructs the value from the list of 'PersistValue'
  fromEntityPersistValues :: PersistBackend m => [PersistValue] -> m (v, [PersistValue])
  -- | Returns constructor number and a list of uniques names and corresponding field values
  getUniques :: DbDescriptor db => Proxy db -> v -> (Int, [(String, [PersistValue])])
  -- | Is internally used by FieldLike Field instance
  -- We could avoid this function if class FieldLike allowed FieldLike Fields Data or FieldLike (Fields Data). However that would require additional extensions in user-space code
  entityFieldChain :: Field v c a -> FieldChain

-- | A holder for Unique constraints
data Unique (u :: (* -> *) -> *)
-- | Key marked with this type can have value for any backend
data BackendSpecific
-- | A phantom datatype to make instance head diffirent @c (ConstructorMarker, v)@
data ConstructorMarker v a
-- | A phantom datatype to make instance head diffirent @u (UniqueMarker, v)@
data UniqueMarker v a

-- | A holder for DB type in backend-specific keys
data KeyForBackend db v = (DbDescriptor db, PersistEntity v) => KeyForBackend (AutoKeyType db)

data Proxy a

data HFalse
data HTrue

-- | Represents condition for a query.
data Cond v (c :: (* -> *) -> *) =
    And (Cond v c) (Cond v c)
  | Or  (Cond v c) (Cond v c)
  | Not (Cond v c)
  | forall a b . Compare ExprRelation (Expr v c a) (Expr v c b)

data ExprRelation = Eq | Ne | Gt | Lt | Ge | Le deriving Show

data Update v c = forall f a b . (FieldLike f (RestrictionHolder v c) a) => Update f (Expr v c b)

-- | Defines sort order of a result-set
data Order v c = forall a f . (FieldLike f (RestrictionHolder v c) a) => Asc  f
               | forall a f . (FieldLike f (RestrictionHolder v c) a) => Desc f

type FieldChain = ((String, DbType), [(String, EmbeddedDef)])

-- | Generalises data that can occur in expressions (so far there are Field and SubField).
class Projection f r a => FieldLike f r a | f -> r a where
  -- | It is used to map field to column names. It can be either a column name for a regular field of non-embedded type or a list of this field and the outer fields in reverse order. Eg, fieldChain $ SomeField ~> Tuple2_0Selector may result in Right [(\"val0\", DbString), (\"some\", DbEmbedded False [dbType \"\", dbType True])].
  -- Function fieldChain can be simplified to f v c a -> [(String, DbType)]. Datatype Either is used for optimisation of the common case, eg Field v c Int.
  fieldChain :: f -> FieldChain

class PersistField v => Embedded v where
  data Selector v :: * -> *
  selectorNum :: Selector v a -> Int

infixl 5 ~>
-- | Accesses fields of the embedded datatypes. For example, @SomeField ==. (\"abc\", \"def\") ||. SomeField ~> Tuple2_0Selector ==. \"def\"@
(~>) :: (PersistEntity v, Constructor c, FieldLike f (RestrictionHolder v c) a, Embedded a) => f -> Selector a a' -> SubField v c a'
field ~> sel = case fieldChain field of
  ((name, typ), prefix) -> case typ of
    DbEmbedded emb@(EmbeddedDef _ ts)             -> SubField (ts !! selectorNum sel, (name, emb):prefix)
    DbEntity (Just (emb@(EmbeddedDef _ ts), _)) _ -> SubField (ts !! selectorNum sel, (name, emb):prefix)
    other -> error $ "(~>): cannot get subfield of non-embedded type " ++ show other

newtype SubField v (c :: (* -> *) -> *) a = SubField ((String, DbType), [(String, EmbeddedDef)])

-- | It can be used in expressions like a regular field. Note that the constructor should be specified for the condition.
-- For example, @delete (AutoKeyField `asTypeOf` (undefined :: f v SomeConstructor) ==. k)@
-- or @delete (AutoKeyField ==. k ||. SomeField ==. \"DUPLICATE\")@
data AutoKeyField v c where
  AutoKeyField :: (PersistEntity v, Constructor c) => AutoKeyField v c

data RestrictionHolder v (c :: (* -> *) -> *)

-- | Any data that can be fetched from a database
class Projection p r a | p -> r a where
  -- | It is like a 'fieldChain' for many fields. Difflist is used for concatenation efficiency.
  projectionFieldChains :: p -> [FieldChain] -> [FieldChain]
  -- | It is like 'fromPersistValues'. However, we cannot use it for projections in all cases. For the 'PersistEntity' instances 'fromPersistValues' expects entity id instead of the entity values.
  projectionResult :: PersistBackend m => p -> [PersistValue] -> m (a, [PersistValue])

data SelectOptions v c hasLimit hasOffset hasOrder = SelectOptions {
    condOptions   :: Cond v c
  , limitOptions  :: Maybe Int
  , offsetOptions :: Maybe Int
  , orderOptions  :: [Order v c]
  }

class HasSelectOptions a v c | a -> v c where
  type HasLimit a
  type HasOffset a
  type HasOrder a
  getSelectOptions :: a -> SelectOptions v c (HasLimit a) (HasOffset a) (HasOrder a)

instance HasSelectOptions (Cond v c) v c where
  type HasLimit (Cond v c) = HFalse
  type HasOffset (Cond v c) = HFalse
  type HasOrder (Cond v c) = HFalse
  getSelectOptions a = SelectOptions a Nothing Nothing []

instance HasSelectOptions (SelectOptions v c hasLimit hasOffset hasOrder) v c where
  type HasLimit (SelectOptions v c hasLimit hasOffset hasOrder) = hasLimit
  type HasOffset (SelectOptions v c hasLimit hasOffset hasOrder) = hasOffset
  type HasOrder (SelectOptions v c hasLimit hasOffset hasOrder) = hasOrder
  getSelectOptions = id

limitTo :: (HasSelectOptions a v c, HasLimit a ~ HFalse) => a -> Int -> SelectOptions v c HTrue (HasOffset a) (HasOrder a)
limitTo opts lim = case getSelectOptions opts of
  SelectOptions c _ off ord -> SelectOptions c (Just lim) off ord

offsetBy :: (HasSelectOptions a v c, HasOffset a ~ HFalse) => a -> Int -> SelectOptions v c (HasLimit a) HTrue (HasOrder a)
offsetBy opts off = case getSelectOptions opts of
  SelectOptions c lim _ ord -> SelectOptions c lim (Just off) ord

orderBy :: (HasSelectOptions a v c, HasOrder a ~ HFalse) => a -> [Order v c] -> SelectOptions v c (HasLimit a) (HasOffset a) HTrue
orderBy opts ord = case getSelectOptions opts of
  SelectOptions c lim off _ -> SelectOptions c lim off ord

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

class PrimitivePersistField (AutoKeyType a) => DbDescriptor a where
  -- | Type of the database default autoincremented key. For example, Sqlite has Int64
  type AutoKeyType a

class (Monad m, DbDescriptor (PhantomDb m)) => PersistBackend m where
  -- | A token which defines the DB type. For example, different monads working with Sqlite, return Sqlite type.
  type PhantomDb m
  -- | Insert a new record to a database and return its 'Key'
  insert        :: PersistEntity v => v -> m (AutoKey v)
  -- | Try to insert a record and return Right newkey. If there is a constraint violation for the given constraint, Left oldkey is returned
  -- , where oldkey is an identifier of the record with the matching values.
  insertBy      :: (PersistEntity v, IsUniqueKey (Key v (Unique u))) => u (UniqueMarker v) -> v -> m (Either (AutoKey v) (AutoKey v))
  -- | Try to insert a record and return Right newkey. If there is a constraint violation for any constraint, Left oldkey is returned
  -- , where oldkey is an identifier of the record with the matching values. Note that if several constraints are violated, a key of an arbitrary matching record is returned.
  insertByAll   :: PersistEntity v => v -> m (Either (AutoKey v) (AutoKey v))
  -- | Replace a record with the given autogenerated key. Result is undefined if the record does not exist.
  replace       :: (PersistEntity v, PrimitivePersistField (Key v BackendSpecific)) => Key v BackendSpecific -> v -> m ()
  -- | Return a list of the records satisfying the condition
  select        :: (PersistEntity v, Constructor c, HasSelectOptions opts v c)
                => opts -> m [v]
  -- | Return a list of all records. Order is undefined. It is useful for datatypes with multiple constructors.
  selectAll     :: PersistEntity v => m [(AutoKey v, v)]
  -- | Fetch an entity from a database
  get           :: (PersistEntity v, PrimitivePersistField (Key v BackendSpecific)) => Key v BackendSpecific -> m (Maybe v)
  -- | Fetch an entity from a database by its unique key
  getBy         :: (PersistEntity v, IsUniqueKey (Key v (Unique u))) => Key v (Unique u) -> m (Maybe v)
  -- | Update the records satisfying the condition
  update        :: (PersistEntity v, Constructor c) => [Update v c] -> Cond v c -> m ()
  -- | Remove the records satisfying the condition
  delete        :: (PersistEntity v, Constructor c) => Cond v c -> m ()
  -- | Remove the record with given key. No-op if the record does not exist
  deleteByKey   :: (PersistEntity v, PrimitivePersistField (Key v BackendSpecific)) => Key v BackendSpecific -> m ()
  -- | Count total number of records satisfying the condition
  count         :: (PersistEntity v, Constructor c) => Cond v c -> m Int
  -- | Count total number of records with all constructors
  countAll      :: PersistEntity v => v -> m Int
  -- | Fetch projection of some fields
  project       :: (PersistEntity v, Constructor c, Projection p (RestrictionHolder v c) a', HasSelectOptions opts v c)
                => p
                -> opts
                -> m [a']
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
  -- | Autokey name if any
  , constrAutoKeyName :: Maybe String
  -- | Parameter names with their named type
  , constrParams  :: [(String, DbType)]
  -- | Uniqueness constraints on the constructor fiels
  , constrUniques :: [UniqueDef]
} deriving (Show, Eq)

-- | Phantom constructors are made instances of this class. This class should be used only by Template Haskell codegen
class Constructor c where
  -- returning ConstructorDef seems more logical, but it would require the value datatype
  -- it can be supplied either as a part of constructor type, eg instance Constructor (MyDataConstructor (MyData a)) which requires -XFlexibleInstances
  -- or as a separate type, eg instance Constructor MyDataConstructor (MyData a) which requires -XMultiParamTypeClasses
  -- the phantoms are primarily used to get the constructor name. So to keep user code cleaner we return only the name and number, which can be later used to get ConstructorDef from the EntityDef
  phantomConstrName :: c (a :: * -> *) -> String
  phantomConstrNum :: c (a :: * -> *) -> Int

class (Constructor (UniqueConstr uKey), PurePersistField uKey) => IsUniqueKey uKey where
  type UniqueConstr uKey :: (* -> *) -> *
  extractUnique :: uKey ~ Key v u => v -> uKey
  uniqueNum :: uKey -> Int

-- | Unique name and list of the field names that form a unique combination.
-- Only fields of 'PrimitivePersistField' types can be used in a unique definition
data UniqueDef = UniqueDef {
    uniqueName :: String
  , uniqueFields :: [(String, DbType)]
}  deriving (Show, Eq)

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
            | DbDayTimeZoned
            | DbBlob    -- ByteString
-- More complex types
            | DbMaybe DbType
            | DbList String DbType -- list name and type of its argument
            | DbEmbedded EmbeddedDef
            -- Nothing means autokey, Just contains a unique key definition and a name of unique constraint.
            | DbEntity (Maybe (EmbeddedDef, String)) EntityDef
  deriving (Eq, Show)

-- | The first argument is a flag which defines if the field names should be concatenated with the outer field name (False) or used as is which provides full control over table column names (True).
-- Value False should be the default value so that a datatype can be embedded without name conflict concern. The second argument list of field names and field types.
data EmbeddedDef = EmbeddedDef Bool [(String, DbType)] deriving (Eq, Show)

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
                  | PersistZonedTime ZT
                  | PersistNull
  deriving (Eq, Show)

-- | Avoid orphan instances.
newtype ZT = ZT ZonedTime deriving (Show, Read)

instance Eq ZT where
    ZT a == ZT b = zonedTimeToLocalTime a == zonedTimeToLocalTime b && zonedTimeZone a == zonedTimeZone b
instance Ord ZT where
    ZT a `compare` ZT b = zonedTimeToUTC a `compare` zonedTimeToUTC b

-- | Arithmetic expressions which can include fields and literals
data Arith v c a =
    Plus  (Arith v c a) (Arith v c a)
  | Minus (Arith v c a) (Arith v c a)
  | Mult  (Arith v c a) (Arith v c a)
  | Abs   (Arith v c a)
  | forall f . (FieldLike f (RestrictionHolder v c) a) => ArithField f
  | Lit   Int64

instance (PersistEntity v, Constructor c) => Eq (Arith v c a) where
  (Plus a1 b1)   == (Plus a2 b2)   = a1 == a2 && b1 == b2
  (Minus a1 b1)  == (Minus a2 b2)  = a1 == a2 && b1 == b2
  (Mult a1 b1)   == (Mult a2 b2)   = a1 == a2 && b1 == b2
  (Abs a)        == (Abs b)        = a == b
  (ArithField a) == (ArithField b) = fieldChain a == fieldChain b
  (Lit a)        == (Lit b)        = a == b
  _              == _              = False

instance (PersistEntity v, Constructor c) => Show (Arith v c a) where
  show (Plus a b)     = "Plus (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Minus a b)    = "Minus (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Mult a b)     = "Mult (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Abs a)        = "Abs (" ++ show a ++ ")"
  show (ArithField a) = "ArithField " ++ show (fieldChain a)
  show (Lit a)        = "Lit " ++ show a

instance (PersistEntity v, Constructor c, Numeric a) => Num (Arith v c a) where
  a + b       = Plus  a b
  a - b       = Minus a b
  a * b       = Mult  a b
  abs         = Abs
  signum      = error "no signum"
  fromInteger = Lit . fromInteger
  
-- | Convert field to an arithmetic value
toArith :: (PersistEntity v, FieldLike f (RestrictionHolder v c) a') => f -> Arith v c a'
toArith = ArithField

-- | Constraint for use in arithmetic expressions. 'Num' is not used to explicitly include only types supported by the library.
-- TODO: consider replacement with 'Num'
class Numeric a

-- | Types which when converted to 'PersistValue' are never NULL.
-- Consider the type @Maybe (Maybe a)@. Now Nothing is stored as NULL, so we cannot distinguish between Just Nothing and Nothing which is a problem.
-- The purpose of this class is to ban the inner Maybe's.
-- Maybe this class can be removed when support for inner Maybe's appears.
class NeverNull a

-- | Datatypes which can be converted directly to 'PersistValue'. The no-value parameter @DbDescriptor db => Proxy db@ allows conversion depend the database details while keeping it pure.
class (SinglePersistField a, PurePersistField a) => PrimitivePersistField a where
  toPrim :: DbDescriptor db => Proxy db -> a -> PersistValue
  fromPrim :: DbDescriptor db => Proxy db -> PersistValue -> a

-- | Used to uniformly represent fields, literals and arithmetic expressions.
-- A value should be converted to 'Expr' for usage in expressions
data Expr v c a where
  ExprField :: (PersistEntity v, FieldLike f (RestrictionHolder v c) a') => f -> Expr v c f
  ExprArith :: PersistEntity v => Arith v c a -> Expr v c (Arith v c a)
  ExprPure :: forall v c a . PurePersistField a => a -> Expr v c a

-- | Represents everything which can be put into a database. This data can be stored in multiple columns and tables. To get value of those columns we might need to access another table. That is why the result type is monadic.
class PersistField a where
  -- | Return name of the type. If it is polymorhic, the names of parameter types are separated with 'Database.Groundhog.Generic.delim' symbol
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
  toPurePersistValues :: DbDescriptor db => Proxy db -> a -> ([PersistValue] -> [PersistValue])
  fromPurePersistValues :: DbDescriptor db => Proxy db -> [PersistValue] -> (a, [PersistValue])

delim :: Char
delim = '#'
