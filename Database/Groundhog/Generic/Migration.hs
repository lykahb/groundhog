-- | This helper module is intended for use by the backend creators
module Database.Groundhog.Generic.Migration
  ( Column(..)
  , UniqueDef'(..)
  , Reference
  , TableInfo
  , AlterColumn(..)
  , AlterColumn'
  , AlterTable(..)
  , AlterDB(..)
  , mkColumns
  , migrateRecursively
  ) where

import Database.Groundhog.Core
import Database.Groundhog.Generic

import Control.Arrow ((***))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), gets, modify)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Describes a database column. Field cType always contains DbType that maps to one column (no DbEmbedded)
data Column typ = Column
    { colName :: String
    , colNull :: Bool
    , colType :: typ
    , colDefault :: Maybe String
    } deriving (Eq, Show)

-- | Foreign table name and names of the corresponding columns
type Reference = (String, [(String, String)])

-- | primary key name, columns, uniques, and references wtih constraint names
type TableInfo typ = (Maybe String, [Column typ], [UniqueDef'], [(Maybe String, Reference)])

data AlterColumn = Type DbType | IsNull | NotNull | Add (Column DbType) | Drop | AddPrimaryKey
                 | Default String | NoDefault | UpdateValue String deriving Show

type AlterColumn' = (String, AlterColumn)

data AlterTable = AddUniqueConstraint String [String]
                | DropConstraint String
                | AddReference Reference
                | DropReference String
                | AlterColumn AlterColumn' deriving Show

data AlterDB typ = AddTable String
                 -- | Table name, create statement, structure of table from DB, structure of table from datatype, alters
                 | AlterTable String String (TableInfo typ) (TableInfo DbType) [AlterTable]
                 -- | Trigger name, table name
                 | DropTrigger String String
                 -- | Trigger name, table name, body
                 | AddTriggerOnDelete String String String
                 -- | Trigger name, table name, field name, body
                 | AddTriggerOnUpdate String String String String
                 | CreateOrReplaceFunction String
                 | DropFunction String
  deriving Show

data UniqueDef' = UniqueDef' String [String] deriving Show

mkColumns :: (DbType -> typ) -> String -> DbType -> ([Column typ], [Reference])
mkColumns mkType columnName dbtype = go "" (columnName, dbtype) where
  go prefix (fname, typ) = (case typ of
    DbEmbedded (EmbeddedDef False ts) -> concatMap' (go $ prefix ++ fname ++ [delim]) ts
    DbEmbedded (EmbeddedDef True  ts) -> concatMap' (go "") ts
    DbMaybe a      -> case go prefix (fname, a) of
      ([c], refs) -> ([c {colNull = True}], refs)
      _ -> error $ "mkColumns: datatype inside DbMaybe must be one column " ++ show a
    DbEntity (Just (emb, uName)) e  -> (cols, ref:refs) where
      (cols, refs) = go prefix (fname, DbEmbedded emb)
      ref = (entityName e, zipWith' (curry $ colName *** colName) cols foreignColumns)
      cDef = case constructors e of
        [cDef'] -> cDef'
        _       -> error "mkColumns: datatype with unique key cannot have more than one constructor"
      UniqueDef _ uFields = findOne "unique" id uniqueName uName $ constrUniques cDef
      fields = map (\(fName, _) -> findOne "field" id fst fName $ constrParams cDef) uFields
      (foreignColumns, _) = concatMap' (go "") fields
    t@(DbEntity Nothing e) -> ([Column name False (mkType t) Nothing], refs) where
      refs = [(entityName e, [(name, keyName)])]
      keyName = case constructors e of
        [cDef] -> fromMaybe (error "mkColumns: autokey name is Nothing") $ constrAutoKeyName cDef
        _      -> "id"
    t@(DbList lName _) -> ([Column name False (mkType t) Nothing], refs) where
      refs = [(lName, [(name, "id")])]
    t -> ([Column name False (mkType t) Nothing], [])) where
      name = prefix ++ fname
      concatMap' f xs = concat *** concat $ unzip $ map f xs
      zipWith' _ [] [] = []
      zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys
      zipWith' _ _ _ = error "mkColumns: the lists have different length"

-- | Create migration for a given entity and all entities it depends on.
-- The stateful Map is used to avoid duplicate migrations when an entity type
-- occurs several times in a datatype
migrateRecursively :: (Monad m, PersistEntity e) => 
     (EntityDef -> m SingleMigration) -- ^ migrate entity
  -> (DbType    -> m SingleMigration) -- ^ migrate list
  -> e                                -- ^ initial entity
  -> StateT NamedMigrations m ()
migrateRecursively migE migL = go . dbType where
  go l@(DbList lName t) = f lName (migL l) (go t)
  go (DbEntity _ e)     = f (entityName e) (migE e) (mapM_ go (allSubtypes e))
  go (DbMaybe t)        = go t
  go (DbEmbedded (EmbeddedDef _ ts))  = mapM_ (go . snd) ts
  go _                  = return ()    -- ordinary types need not migration
  f name mig cont = do
    v <- gets (Map.lookup name)
    case v of
      Nothing -> lift mig >>= modify . Map.insert name >> cont
      _ -> return ()
  allSubtypes = map snd . concatMap constrParams . constructors
