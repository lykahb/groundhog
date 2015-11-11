{-# LANGUAGE RecordWildCards, TypeFamilies #-}
-- | This helper module is intended for use by the backend creators
module Database.Groundhog.Generic.Migration
  ( Column(..)
  , Reference(..)
  , QualifiedName
  , TableInfo(..)
  , UniqueDefInfo
  , AlterColumn(..)
  , AlterTable(..)
  , AlterDB(..)
  , MigrationPack(..)
  , SchemaAnalyzer(..)
  , migrateRecursively
  , migrateSchema
  , migrateEntity
  , migrateList
  , getAlters
  , defaultMigConstr
  , showReferenceAction
  , readReferenceAction
  ) where

import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql (flatten, mainTableName, tableName)

import Control.Applicative (Applicative)
import Control.Arrow ((***), (&&&))
import Control.Monad (liftM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (gets, modify)
import Data.Either (lefts)
import Data.Function (on)
import qualified Data.Map as Map
import Data.List (group, intercalate)
import Data.Maybe (fromMaybe, mapMaybe)

data Column = Column
    { colName :: String
    , colNull :: Bool
    , colType :: DbTypePrimitive
    , colDefault :: Maybe String
    } deriving (Eq, Show)

data Reference = Reference {
    referencedTableName :: QualifiedName
  , referencedColumns :: [(String, String)] -- ^ child column, parent column
  , referenceOnDelete :: Maybe ReferenceActionType
  , referenceOnUpdate :: Maybe ReferenceActionType
  } deriving Show

-- | Schema-qualified name of a table of another database object
type QualifiedName = (Maybe String, String)

-- | Either column name or expression
type UniqueDefInfo = UniqueDef' String (Either String String)
data TableInfo = TableInfo {
    tableColumns :: [Column]
  , tableUniques :: [UniqueDefInfo]
    -- | constraint name and reference
  , tableReferences :: [(Maybe String, Reference)]
} deriving Show

data AlterColumn = Type DbTypePrimitive | IsNull | NotNull
                 | Default String | NoDefault | UpdateValue String deriving Show

data AlterTable = AddUnique UniqueDefInfo
                | DropConstraint String
                | DropIndex String
                | AddReference Reference
                | DropReference String
                | DropColumn String
                | AddColumn Column
                | AlterColumn Column [AlterColumn] deriving Show

data AlterDB = AddTable String
             -- | Qualified table name, create statement, structure of table from DB, structure of table from datatype, alters
             | AlterTable QualifiedName String TableInfo TableInfo [AlterTable]
             -- | Qualified trigger name, qualified table name
             | DropTrigger QualifiedName QualifiedName
             -- | Qualified trigger name, qualified table name, body
             | AddTriggerOnDelete QualifiedName QualifiedName String
             -- | Qualified trigger name, qualified table name, field name, body
             | AddTriggerOnUpdate QualifiedName QualifiedName (Maybe String) String
             -- | Statement which creates the function
             | CreateOrReplaceFunction String
             -- | Qualified function name
             | DropFunction QualifiedName
             -- | Schema name, if not exists
             | CreateSchema String Bool
  deriving Show

data MigrationPack conn = MigrationPack {
    compareTypes :: DbTypePrimitive -> DbTypePrimitive -> Bool
  , compareRefs :: (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
  , compareUniqs :: UniqueDefInfo -> UniqueDefInfo -> Bool
  , compareDefaults :: String -> String -> Bool
  , migTriggerOnDelete :: QualifiedName -> [(String, String)] -> Action conn (Bool, [AlterDB])
  , migTriggerOnUpdate :: QualifiedName -> [(String, String)] -> Action conn [(Bool, [AlterDB])]
  , migConstr :: EntityDef -> ConstructorDef -> Action conn (Bool, SingleMigration)
  , escape :: String -> String
  , autoincrementedKeyTypeName :: String
  , mainTableId :: String
  , defaultPriority :: Int
  -- | Sql pieces for the create table statement that add constraints and alterations for running after the table is created
  , addUniquesReferences :: [UniqueDefInfo] -> [Reference] -> ([String], [AlterTable])
  , showSqlType :: DbTypePrimitive -> String
  , showColumn :: Column -> String
  , showAlterDb :: AlterDB -> SingleMigration
  , defaultReferenceOnDelete :: ReferenceActionType
  , defaultReferenceOnUpdate :: ReferenceActionType
}

mkColumns :: DbTypePrimitive -> (String, DbType) -> ([Column] -> [Column])
mkColumns listAutoKeyType field = go "" field where
  go prefix (fname, typ) acc = (case typ of
    DbEmbedded (EmbeddedDef flag ts) _ -> foldr (go prefix') acc ts where prefix' = if flag then "" else prefix ++ fname ++ [delim]
    DbList _ _ -> Column fullName False listAutoKeyType Nothing : acc
    DbTypePrimitive t nullable def _ -> Column fullName nullable t def : acc) where
      fullName = prefix ++ fname

mkReferences :: DbTypePrimitive -> (String, DbType) -> [Reference]
mkReferences autoKeyType field = concat $ traverseDbType f field where
  f (DbEmbedded _ ref) cols = maybe [] (return . mkReference cols) ref
  f (DbList lName _) cols = [mkReference cols (Right ((Nothing, lName), ["id"]), Nothing, Nothing)]
  f (DbTypePrimitive _ _ _ ref) cols = maybe [] (return . mkReference cols) ref
  mkReference :: [String] -> ParentTableReference -> Reference
  mkReference cols (parent, onDel, onUpd) = case parent of
    Left (e, Nothing) -> Reference (entitySchema e, entityName e) (zipWith' (,) cols [keyName]) onDel onUpd where
      keyName = case constructors e of
        [cDef] -> fromMaybe (error "mkReferences: autokey name is Nothing") $ constrAutoKeyName cDef
        _      -> "id"
    Left (e, (Just uName)) -> Reference (entitySchema e, entityName e) (zipWith' (,) cols (map colName parentColumns)) onDel onUpd where
      cDef = case constructors e of
        [cDef'] -> cDef'
        _       -> error "mkReferences: datatype with unique key cannot have more than one constructor"
      uDef = findOne "unique" uniqueDefName (Just uName) $ constrUniques cDef
      fields = map (\(fName, _) -> findOne "field" fst fName $ constrParams cDef) $ getUniqueFields uDef
      parentColumns = foldr (mkColumns autoKeyType) [] fields
    Right (parentTable, parentColumns) -> Reference parentTable (zipWith' (,) cols parentColumns) onDel onUpd
  zipWith' _ [] [] = []
  zipWith' g (x:xs) (y:ys) = g x y: zipWith' g xs ys
  zipWith' _ _ _ = error "mkReferences: the lists have different length"

traverseDbType :: (DbType -> [String] -> a) -> (String, DbType) -> [a]
traverseDbType f field = snd $ go "" field where
  go prefix (fname, typ) = (case typ of
    t@(DbEmbedded (EmbeddedDef flag ts) _) -> (cols, f t cols : xs) where
      prefix' = if flag then "" else prefix ++ fname ++ [delim]
      (cols, xs) = concatMap' (go prefix') ts
    t@(DbList _ _) -> ([name], [f t [name]])
    t@(DbTypePrimitive _ _ _ _) -> ([name], [f t [name]])) where
      name = prefix ++ fname
      concatMap' g xs = concat *** concat $ unzip $ map g xs

-- | Create migration for a given entity and all entities it depends on.
-- The stateful Map is used to avoid duplicate migrations when an entity type
-- occurs several times in a datatype
migrateRecursively :: (PersistBackend m, PersistEntity v) => 
     (String    -> m SingleMigration)    -- ^ migrate schema
  -> (EntityDef -> m SingleMigration) -- ^ migrate entity
  -> (DbType    -> m SingleMigration) -- ^ migrate list
  -> v                                -- ^ initial entity
  -> Migration m
migrateRecursively migS migE migL v = result where
  result = migEntity $ entityDef proxy v
  proxy = (undefined :: t m a -> proxy (Conn m)) result
  go l@(DbList lName t) = f ("list " ++ lName) (migL l) (go t)
  go (DbEmbedded (EmbeddedDef _ ts) ref) = mapM_ (go . snd) ts >> migRef ref
  go (DbTypePrimitive _ _ _ ref) = migRef ref
  allSubtypes = map snd . concatMap constrParams . constructors
  migRef ref = case ref of
    Just (Left (e, _), _, _) -> migEntity e
    _ -> return ()
  migEntity e = do
    case entitySchema e of
      Just name -> f ("schema " ++ name) (migS name) (return ())
      Nothing -> return ()
    f ("entity " ++ mainTableName id e) (migE e) (mapM_ go (allSubtypes e))
  f name mig cont = do
    a <- gets (Map.lookup name)
    when (a == Nothing) $
      lift mig >>= modify . Map.insert name >> cont

migrateSchema :: SchemaAnalyzer conn => MigrationPack conn -> String -> Action conn SingleMigration
migrateSchema MigrationPack{..} schema = do
  x <- schemaExists schema
  return $ if x
    then Right []
    else showAlterDb $ CreateSchema schema False

migrateEntity :: (SchemaAnalyzer conn, PersistBackendConn conn) => MigrationPack conn -> EntityDef -> Action conn SingleMigration
migrateEntity m@MigrationPack{..} e = do
  autoKeyType <- fmap getDefaultAutoKeyType phantomDb
  let name = entityName e
      constrs = constructors e
      mainTableQuery = "CREATE TABLE " ++ escape name ++ " (" ++ mainTableId ++ " " ++ autoincrementedKeyTypeName ++ ", discr INTEGER NOT NULL)"
      expectedMainStructure = TableInfo [Column "id" False autoKeyType Nothing, Column "discr" False DbInt32 Nothing] [UniqueDef Nothing (UniquePrimary True) [Left "id"]] []

  if isSimple constrs
    then do
      x <- analyzeTable (entitySchema e, name)
      -- check whether the table was created for multiple constructors before
      case x of
        Just old | null $ getAlters m old expectedMainStructure -> return $ Left
          ["Datatype with multiple constructors was truncated to one constructor. Manual migration required. Datatype: " ++ name]
        _ -> liftM snd $ migConstr e $ head constrs
    else do
      mainStructure <- analyzeTable (entitySchema e, name)
      let constrTable c = name ++ [delim] ++ constrName c
      res <- mapM (migConstr e) constrs
      return $ case mainStructure of
        Nothing -> 
          -- no constructor tables can exist if there is no main data table
          let orphans = filter (fst . fst) $ zip res constrs
          in if null orphans
            then mergeMigrations $ Right [(False, defaultPriority, mainTableQuery)]:map snd res
            else Left $ map (\(_, c) -> "Orphan constructor table found: " ++ constrTable c) orphans
        Just mainStructure' -> if null $ getAlters m mainStructure' expectedMainStructure
          then let
               -- the datatype had also many constructors before
               -- check whether any new constructors appeared and increment older discriminators, which were shifted by newer constructors inserted not in the end
               updateDiscriminators = go 0 . map (head &&& length) . group . map fst $ res where
                  go acc ((False, n):(True, n2):xs) = (False, defaultPriority, "UPDATE " ++ escape name ++ " SET discr = discr + " ++ show n ++ " WHERE discr >= " ++ show acc) : go (acc + n + n2) xs
                  go acc ((True, n):xs) = go (acc + n) xs
                  go _ _ = []
               in mergeMigrations $ Right updateDiscriminators: map snd res
          else Left ["Unexpected structure of main table for Datatype: " ++ name ++ ". Table info: " ++ show mainStructure']

migrateList :: (SchemaAnalyzer conn, PersistBackendConn conn) => MigrationPack conn -> DbType -> Action conn SingleMigration
migrateList m@MigrationPack{..} (DbList mainName t) = do
  autoKeyType <- fmap getDefaultAutoKeyType phantomDb
  let valuesName = mainName ++ delim : "values"
      (valueCols, valueRefs) = (($ []) . mkColumns autoKeyType) &&& mkReferences autoKeyType $ ("value", t)
      refs' = Reference (Nothing, mainName) [("id", "id")] (Just Cascade) Nothing : valueRefs
      expectedMainStructure = TableInfo [Column "id" False autoKeyType Nothing] [UniqueDef Nothing (UniquePrimary True) [Left "id"]] []
      mainQuery = "CREATE TABLE " ++ escape mainName ++ " (id " ++ autoincrementedKeyTypeName ++ ")"
      (addInCreate, addInAlters) = addUniquesReferences [] refs'
      expectedValuesStructure = TableInfo valueColumns [] (map (\x -> (Nothing, x)) refs')
      valueColumns = Column "id" False autoKeyType Nothing : Column "ord" False DbInt32 Nothing : valueCols
      valuesQuery = "CREATE TABLE " ++ escape valuesName ++ " (" ++ intercalate ", " (map showColumn valueColumns ++ addInCreate) ++ ")"
  -- TODO: handle case when outer entity has a schema
  mainStructure <- analyzeTable (Nothing, mainName)
  valuesStructure <- analyzeTable (Nothing, valuesName)
  let triggerMain = []
  (_, triggerValues) <- migTriggerOnDelete (Nothing, valuesName) $ mkDeletes escape ("value", t)
  return $ case (mainStructure, valuesStructure) of
    (Nothing, Nothing) -> let
      rest = [AlterTable (Nothing, valuesName) valuesQuery expectedValuesStructure expectedValuesStructure addInAlters]
      in mergeMigrations $ map showAlterDb $ [AddTable mainQuery, AddTable valuesQuery] ++ rest ++ triggerMain ++ triggerValues
    (Just mainStructure', Just valuesStructure') -> let
      f name a b = if null $ getAlters m a b
        then []
        else ["List table " ++ name ++ " error. Expected: " ++ show b ++ ". Found: " ++ show a]
      errors = f mainName mainStructure' expectedMainStructure ++ f valuesName valuesStructure' expectedValuesStructure
      in if null errors then Right [] else Left errors
    (_, Nothing) -> Left ["Found orphan main list table " ++ mainName]
    (Nothing, _) -> Left ["Found orphan list values table " ++ valuesName]
migrateList _ t = fail $ "migrateList: expected DbList, got " ++ show t

getAlters :: MigrationPack m
          -> TableInfo -- ^ From database
          -> TableInfo -- ^ From datatype
          -> [AlterTable]
getAlters m@MigrationPack{..} (TableInfo oldColumns oldUniques oldRefs) (TableInfo newColumns newUniques newRefs) = tableAlters
  where
    (oldOnlyColumns, newOnlyColumns, commonColumns) = matchElements ((==) `on` colName) oldColumns newColumns
    (oldOnlyUniques, newOnlyUniques, commonUniques) = matchElements compareUniqs oldUniques newUniques
    (oldOnlyRefs, newOnlyRefs, _) = matchElements compareRefs oldRefs newRefs
    primaryColumns = lefts $ concatMap uniqueDefFields $ filter ((== UniquePrimary True) . uniqueDefType) oldUniques

    colAlters = mapMaybe (\(a, b) -> mkAlterColumn b $ migrateColumn m a b) (filter ((`notElem` primaryColumns) . colName . fst) commonColumns)
    mkAlterColumn col alters = if null alters then Nothing else Just $ AlterColumn col alters
    tableAlters = 
         map (DropColumn . colName) oldOnlyColumns
      ++ map AddColumn newOnlyColumns
      ++ colAlters
      ++ map dropUnique oldOnlyUniques
      ++ map AddUnique newOnlyUniques
      ++ concatMap (uncurry migrateUniq) commonUniques
      ++ map (DropReference . fromMaybe (error "getAlters: old reference does not have name") . fst) oldOnlyRefs
      ++ map (AddReference . snd) newOnlyRefs

-- from database, from datatype
migrateColumn :: MigrationPack m -> Column -> Column -> [AlterColumn]
migrateColumn MigrationPack{..} (Column _ isNull1 type1 def1) (Column _ isNull2 type2 def2) = modDef ++ modNull ++ modType where
  modNull = case (isNull1, isNull2) of
    (False, True) -> [IsNull]
    (True, False) -> case def2 of
      Nothing -> [NotNull]
      Just s -> [UpdateValue s, NotNull]
    _ -> []
  modType = if compareTypes type1 type2 then [] else [Type type2]
  modDef = case (def1, def2) of
    (Nothing, Nothing) -> []
    (Just def1', Just def2') | compareDefaults def1' def2' -> []
    _ -> [maybe NoDefault Default def2]

-- from database, from datatype
migrateUniq :: UniqueDefInfo -> UniqueDefInfo -> [AlterTable]
migrateUniq u1@(UniqueDef _ _ cols1) u2@(UniqueDef _ _ cols2) = if haveSameElems (==) cols1 cols2
  then []
  else [dropUnique u1, AddUnique u2]

dropUnique :: UniqueDefInfo -> AlterTable
dropUnique (UniqueDef name typ _) = (case typ of
  UniqueConstraint -> DropConstraint name'
  UniqueIndex -> DropIndex name'
  UniquePrimary _ -> DropConstraint name') where
  name' = fromMaybe (error $ "dropUnique: constraint which should be dropped does not have a name") name
  
defaultMigConstr :: (SchemaAnalyzer conn, PersistBackendConn conn) => MigrationPack conn -> EntityDef -> ConstructorDef -> Action conn (Bool, SingleMigration)
defaultMigConstr m@MigrationPack{..} e constr = do
  let simple = isSimple $ constructors e
      name = entityName e
      qualifiedCName = (entitySchema e, if simple then name else name ++ [delim] ++ constrName constr)
  autoKeyType <- fmap getDefaultAutoKeyType phantomDb
  tableStructure <- analyzeTable qualifiedCName
  let dels = concatMap (mkDeletes escape) $ constrParams constr
  (triggerExisted, delTrigger) <- migTriggerOnDelete qualifiedCName dels
  updTriggers <- liftM (concatMap snd) $ migTriggerOnUpdate qualifiedCName dels

  let (expectedTableStructure, (addTable, addInAlters)) = (case constrAutoKeyName constr of
        Nothing -> (TableInfo columns uniques (mkRefs refs), f [] columns uniques refs)
        Just keyName -> let keyColumn = Column keyName False autoKeyType Nothing 
                        in if simple
          then (TableInfo (keyColumn:columns) (uniques ++ [UniqueDef Nothing (UniquePrimary True) [Left keyName]]) (mkRefs refs)
               , f [escape keyName ++ " " ++ autoincrementedKeyTypeName] columns uniques refs)
          else let columns' = keyColumn:columns
                   refs' = refs ++ [Reference (entitySchema e, name) [(keyName, mainTableId)] (Just Cascade) Nothing]
                   uniques' = uniques ++ [UniqueDef Nothing UniqueConstraint [Left keyName]]
               in (TableInfo columns' uniques' (mkRefs refs'), f [] columns' uniques' refs')) where
        (columns, refs) = foldr (mkColumns autoKeyType) [] &&& concatMap (mkReferences autoKeyType) $ constrParams constr
        uniques = map (\u -> u {uniqueDefFields = concatMap (either (map Left . ($ []) . flatten id) (return . Right)) $ uniqueDefFields u}) $ constrUniques constr
        f autoKey cols uniqs refs' = (addTable', addInAlters') where
          (addInCreate, addInAlters') = addUniquesReferences uniqs refs'
          items = autoKey ++ map showColumn cols ++ addInCreate
          addTable' = "CREATE TABLE " ++ tableName escape e constr ++ " (" ++ intercalate ", " items ++ ")"
        mkRefs = map (\r -> (Nothing, r))

      (migErrs, constrExisted, mig) = case tableStructure of
        Nothing -> let
          rest = AlterTable qualifiedCName addTable expectedTableStructure expectedTableStructure addInAlters
          in ([], False, [AddTable addTable, rest])
        Just oldTableStructure -> let
          alters = getAlters m oldTableStructure expectedTableStructure
          alterTable = if null alters
            then []
            else [AlterTable qualifiedCName addTable oldTableStructure expectedTableStructure alters]
          in ([], True, alterTable)
      -- this can happen when an ephemeral field was added. Consider doing something else except throwing an error
      allErrs = if constrExisted == triggerExisted || (constrExisted && null dels)
        then migErrs
        else ["Both trigger and constructor table must exist: " ++ show qualifiedCName] ++ migErrs
  return $ (constrExisted, if null allErrs
    then mergeMigrations $ map showAlterDb $ mig ++ delTrigger ++ updTriggers
    else Left allErrs)

-- on delete removes all ephemeral data
-- returns column name and delete statement for the referenced table
mkDeletes :: (String -> String) -> (String, DbType) -> [(String, String)]
mkDeletes esc = concat . traverseDbType f where
  f (DbList ref _) [col] = [(col, "DELETE FROM " ++ esc ref ++ " WHERE id=old." ++ esc col ++ ";")]
  f _ _ = []

showReferenceAction :: ReferenceActionType -> String
showReferenceAction NoAction = "NO ACTION"
showReferenceAction Restrict = "RESTRICT"
showReferenceAction Cascade = "CASCADE"
showReferenceAction SetNull = "SET NULL"
showReferenceAction SetDefault = "SET DEFAULT"

readReferenceAction :: String -> Maybe ReferenceActionType
readReferenceAction c = case c of
  "NO ACTION" -> Just NoAction
  "RESTRICT" -> Just Restrict
  "CASCADE" -> Just Cascade
  "SET NULL" -> Just SetNull
  "SET DEFAULT" -> Just SetDefault
  _ -> Nothing

class PersistBackendConn conn => SchemaAnalyzer conn where
  schemaExists :: (PersistBackend m, Conn m ~ conn) => String -- ^ Schema name
               -> m Bool
  getCurrentSchema :: (PersistBackend m, Conn m ~ conn) => m (Maybe String)
  listTables :: (PersistBackend m, Conn m ~ conn) => Maybe String -- ^ Schema name
             -> m [String]
  listTableTriggers :: (PersistBackend m, Conn m ~ conn) => QualifiedName -- ^ Qualified table name
                    -> m [String]
  analyzeTable :: (PersistBackend m, Conn m ~ conn) => QualifiedName -- ^ Qualified table name
               -> m (Maybe TableInfo)
  analyzeTrigger :: (PersistBackend m, Conn m ~ conn) =>  QualifiedName -- ^ Qualified trigger name
                 -> m (Maybe String)
  analyzeFunction :: (PersistBackend m, Conn m ~ conn) => QualifiedName -- ^ Qualified function name
                  -> m (Maybe (Maybe [DbTypePrimitive], Maybe DbTypePrimitive, String)) -- ^ Argument types, return type, and body
  getMigrationPack :: (PersistBackend m, Conn m ~ conn) => m (MigrationPack conn)
