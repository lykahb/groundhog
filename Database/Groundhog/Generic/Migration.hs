{-# LANGUAGE RecordWildCards #-}
-- | This helper module is intended for use by the backend creators
module Database.Groundhog.Generic.Migration
  ( Column(..)
  , UniqueDef'(..)
  , Reference
  , TableInfo(..)
  , AlterColumn(..)
  , AlterColumn'
  , AlterTable(..)
  , AlterDB(..)
  , MigrationPack(..)
  , mkColumns
  , migrateRecursively
  , migrateEntity
  , migrateList
  , getAlters
  , defaultMigConstr
  ) where

import Database.Groundhog.Core
import Database.Groundhog.Generic

import Control.Arrow ((***), (&&&))
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), gets, modify)
import qualified Data.Map as Map
import Data.List (group, intercalate)
import Data.Maybe (fromJust, fromMaybe, mapMaybe, maybeToList)

-- Describes a database column. Field cType always contains DbType that maps to one column (no DbEmbedded)
data Column typ = Column
    { colName :: String
    , colNull :: Bool
    , colType :: typ
    , colDefault :: Maybe String
    } deriving (Eq, Show)

-- | Foreign table name and names of the corresponding columns
type Reference = (String, [(String, String)])

data TableInfo typ = TableInfo {
    tablePrimaryKeyName :: Maybe String
  , tableColumns :: [Column typ]
  , tableUniques :: [UniqueDef']
    -- | constraint name and reference
  , tableReferences :: [(Maybe String, Reference)]
} deriving Show

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

data MigrationPack m typ = MigrationPack {
    compareColumns :: Column typ -> Column DbType -> Bool
  , compareRefs :: (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
  , compareUniqs :: UniqueDef' -> UniqueDef' -> Bool
  , checkTable :: String -> m (Maybe (Either [String] (TableInfo typ)))
  , migTriggerOnDelete :: String -> [(String, String)] -> m (Bool, [AlterDB typ])
  , migTriggerOnUpdate :: String -> String -> String -> m (Bool, [AlterDB typ])
  , migConstr :: MigrationPack m typ -> Bool -> String -> ConstructorDef -> m (Bool, SingleMigration)
  , escape :: String -> String
  , primaryKeyType :: String
  , foreignKeyType :: String
  , mainTableId :: String
  , defaultPriority :: Int
  , convertType :: DbType -> typ
  -- | Sql pieces for the create table statement that add constraints and alterations for running after the table is created
  , addUniquesReferences :: [UniqueDef'] -> [Reference] -> ([String], [AlterTable])
  , showColumn :: Column DbType -> String
  , showAlterDb :: AlterDB typ -> SingleMigration
}

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
migrateRecursively :: (Monad m, PersistEntity v) => 
     (EntityDef -> m SingleMigration) -- ^ migrate entity
  -> (DbType    -> m SingleMigration) -- ^ migrate list
  -> v                                -- ^ initial entity
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

migrateEntity :: (Monad m, Show typ) => MigrationPack m typ -> EntityDef -> m SingleMigration
migrateEntity m@MigrationPack{..} e = do
  let name = entityName e
  let constrs = constructors e
  let mainTableQuery = "CREATE TABLE " ++ escape name ++ " (" ++ mainTableId ++ " " ++ primaryKeyType ++ ", discr INTEGER NOT NULL)"
  let mainTableColumns = [Column "discr" False DbInt32 Nothing]

  if isSimple constrs
    then do
      x <- checkTable name
      -- check whether the table was created for multiple constructors before
      case x of
        Just (Right old) | haveSameElems compareColumns (tableColumns old) mainTableColumns -> do
          return $ Left ["Datatype with multiple constructors was truncated to one constructor. Manual migration required. Datatype: " ++ name]
        Just (Left errs) -> return (Left errs)
        _ -> liftM snd $ migConstr m True name $ head constrs
    else do
      maincolumns <- checkTable name
      let constrTable c = name ++ [delim] ++ constrName c
      res <- mapM (\c -> migConstr m False name c) constrs
      case maincolumns of
        Nothing -> do
          -- no constructor tables can exist if there is no main data table
          let orphans = filter (fst . fst) $ zip res constrs
          return $ if null orphans
            then mergeMigrations $ Right [(False, defaultPriority, mainTableQuery)]:map snd res
            else Left $ map (\(_, c) -> "Orphan constructor table found: " ++ constrTable c) orphans
        Just (Right (TableInfo (Just _) columns [] [])) -> do
          if haveSameElems compareColumns columns mainTableColumns
            then do
              -- the datatype had also many constructors before
              -- check whether any new constructors appeared and increment older discriminators, which were shifted by newer constructors inserted not in the end
              let updateDiscriminators = Right . go 0 . map (head &&& length) . group . map fst $ res where
                  go acc ((False, n):(True, n2):xs) = (False, defaultPriority, "UPDATE " ++ escape name ++ " SET discr = discr + " ++ show n ++ " WHERE discr >= " ++ show acc) : go (acc + n + n2) xs
                  go acc ((True, n):xs) = go (acc + n) xs
                  go _ _ = []
              return $ mergeMigrations $ updateDiscriminators: (map snd res)
            else do
              return $ Left ["Migration from one constructor to many will be implemented soon. Datatype: " ++ name]
        Just (Right structure) -> do
          return $ Left ["Unexpected structure of main table for Datatype: " ++ name ++ ". Table info: " ++ show structure]
        Just (Left errs) -> return (Left errs)

migrateList :: (Monad m, Show typ) => MigrationPack m typ -> DbType -> m SingleMigration
migrateList m@MigrationPack{..} (DbList mainName t) = do
  let valuesName = mainName ++ delim : "values"
  let (valueCols, valueRefs) = mkColumns id "value" t
  let mainQuery = "CREATE TABLE " ++ escape mainName ++ " (id " ++ primaryKeyType ++ ")"
  let (addInCreate, addInAlters) = addUniquesReferences [] valueRefs
  let items = ("id " ++ foreignKeyType ++ " NOT NULL REFERENCES " ++ escape mainName ++ " ON DELETE CASCADE"):"ord INTEGER NOT NULL" : map showColumn valueCols ++ addInCreate
  let valuesQuery = "CREATE TABLE " ++ escape valuesName ++ " (" ++ intercalate ", " items ++ ")"
  let expectedMainStructure = TableInfo (Just "id") [] [] []
  let valueColumns = Column "id" False DbInt64 Nothing : Column "ord" False DbInt32 Nothing : valueCols
  let expectedValuesStructure = TableInfo Nothing valueColumns [] (map (\x -> (Nothing, x)) $ (mainName, [("id", "id")]) : valueRefs)
  mainStructure <- checkTable mainName
  valuesStructure <- checkTable valuesName
  let triggerMain = []
  (_, triggerValues) <- migTriggerOnDelete valuesName $ mkDeletes m valueCols
  return $ case (mainStructure, valuesStructure) of
    (Nothing, Nothing) -> let
      oldValuesStructure = expectedValuesStructure {tableColumns = map (\c -> c {colType = convertType (colType c)}) valueColumns}
      rest = [AlterTable valuesName valuesQuery oldValuesStructure expectedValuesStructure addInAlters]
      in mergeMigrations $ map showAlterDb $ [AddTable mainQuery, AddTable valuesQuery] ++ rest ++ triggerMain ++ triggerValues
    (Just (Right mainStructure'), Just (Right valuesStructure')) -> let
      f name a@(TableInfo id1 cols1 uniqs1 refs1) b@(TableInfo id2 cols2 uniqs2 refs2) = if id1 == id2 && haveSameElems compareColumns cols1 cols2 && haveSameElems compareUniqs uniqs1 uniqs2 && haveSameElems compareRefs refs1 refs2
        then []
        else ["List table " ++ name ++ " error. Expected: " ++ show b ++ ". Found: " ++ show a]
      errors = f mainName mainStructure' expectedMainStructure ++ f valuesName valuesStructure' expectedValuesStructure
      in if null errors then Right [] else Left errors
    (Just (Left errs1), Just (Left errs2)) -> Left $ errs1 ++ errs2
    (Just (Left errs), Just _) -> Left errs
    (Just _, Just (Left errs)) -> Left errs
    (_, Nothing) -> Left ["Found orphan main list table " ++ mainName]
    (Nothing, _) -> Left ["Found orphan list values table " ++ valuesName]
migrateList _ t = fail $ "migrateList: expected DbList, got " ++ show t

-- from database, from datatype
getAlters :: (Eq typ, Show typ) =>
             MigrationPack m typ
          -> TableInfo typ
          -> TableInfo DbType
          -> [AlterTable]
getAlters m@MigrationPack{..} (TableInfo oldId oldColumns oldUniques oldRefs) (TableInfo newId newColumns newUniques newRefs) = map AlterColumn colAlters ++ tableAlters
  where
    (oldOnlyColumns, newOnlyColumns, commonColumns) = matchElements compareColumns oldColumns newColumns
    (oldOnlyUniques, newOnlyUniques, commonUniques) = matchElements compareUniqs oldUniques newUniques
    primaryKeyAlters = case (oldId, newId) of
      (Nothing, Just newName) -> [(newName, AddPrimaryKey)]
      (Just oldName, Nothing) -> [(oldName, Drop)]
      (Just oldName, Just newName) | oldName /= newName -> error $ "getAlters: cannot rename primary key (old " ++ oldName ++ ", new " ++ newName ++ ")"
      _ -> []
    (oldOnlyRefs, newOnlyRefs, _) = matchElements compareRefs oldRefs newRefs

    colAlters = map (\x -> (colName x, Drop)) oldOnlyColumns ++ map (\x -> (colName x, Add x)) newOnlyColumns ++ concatMap (migrateColumn m) commonColumns ++ primaryKeyAlters
    tableAlters = 
         map (\(UniqueDef' name _) -> DropConstraint name) oldOnlyUniques
      ++ map (\(UniqueDef' name cols) -> AddUniqueConstraint name cols) newOnlyUniques
      ++ concatMap migrateUniq commonUniques
      ++ map (DropReference . fromMaybe (error "getAlters: old reference does not have name") . fst) oldOnlyRefs
      ++ map (AddReference . snd) newOnlyRefs

-- from database, from datatype
migrateColumn :: Eq typ => MigrationPack m typ -> (Column typ, Column DbType) -> [AlterColumn']
migrateColumn MigrationPack{..} (Column name1 isNull1 type1 def1, Column _ isNull2 type2 def2) = modDef ++ modNull ++ modType where
  modNull = case (isNull1, isNull2) of
    (False, True) -> [(name1, IsNull)]
    (True, False) -> case def2 of
      Nothing -> [(name1, NotNull)]
      Just s -> [(name1, UpdateValue s), (name1, NotNull)]
    _ -> []
  modType = if type1 == convertType type2 then [] else [(name1, Type type2)]
  modDef = if def1 == def2
    then []
    else [(name1, maybe NoDefault Default def2)]

-- from database, from datatype
migrateUniq :: (UniqueDef', UniqueDef') -> [AlterTable]
migrateUniq (UniqueDef' name1 cols1, UniqueDef' name2 cols2) = if haveSameElems (==) cols1 cols2
  then []
  else [DropConstraint name1, AddUniqueConstraint name2 cols2]
  
defaultMigConstr :: (Monad m, Eq typ, Show typ) => MigrationPack m typ -> Bool -> String -> ConstructorDef -> m (Bool, SingleMigration)
defaultMigConstr migPack@MigrationPack{..} simple name constr = do
  let cName = if simple then name else name ++ [delim] ++ constrName constr
  let mkColumns' xs = concat *** concat $ unzip $ map (uncurry $ mkColumns id) xs
  let (columns, refs) = mkColumns' $ constrParams constr
  tableStructure <- checkTable cName
  let dels = mkDeletes migPack columns
  (triggerExisted, delTrigger) <- migTriggerOnDelete cName dels
  updTriggers <- liftM concat $ mapM (liftM snd . uncurry (migTriggerOnUpdate cName)) dels
  
  let mainTableName = if simple then Nothing else Just name
      refs' = maybeToList (fmap (\x -> (x, [(fromJust $ constrAutoKeyName constr, mainTableId)])) mainTableName) ++ refs

      mainRef = maybe "" (\x -> " REFERENCES " ++ escape x ++ " ON DELETE CASCADE ") mainTableName
      autoKey = fmap (\x -> escape x ++ " " ++ primaryKeyType ++ mainRef) $ constrAutoKeyName constr

      uniques = map (\(UniqueDef uName cols) -> UniqueDef' uName (map colName $ fst $ mkColumns' cols)) $ constrUniques constr
      (addInCreate, addInAlters) = addUniquesReferences uniques refs
      -- refs instead of refs' because the reference to the main table id is hardcoded in mainRef
      items = maybeToList autoKey ++ map showColumn columns ++ addInCreate
      addTable = "CREATE TABLE " ++ escape cName ++ " (" ++ intercalate ", " items ++ ")"

      expectedTableStructure = TableInfo (constrAutoKeyName constr) columns uniques (map (\r -> (Nothing, r)) refs')
      (migErrs, constrExisted, mig) = case tableStructure of
        Nothing  -> let
          oldTableStructure = expectedTableStructure {tableColumns = map (\c -> c {colType = convertType (colType c)}) columns}
          rest = AlterTable cName addTable oldTableStructure expectedTableStructure addInAlters
          in ([], False, [AddTable addTable, rest])
        Just (Right oldTableStructure) -> let
          alters = getAlters migPack oldTableStructure expectedTableStructure
          in ([], True, [AlterTable cName addTable oldTableStructure expectedTableStructure alters])
        Just (Left x) -> (x, True, [])
      -- this can happen when an ephemeral field was added. Consider doing something else except throwing an error
      errs = if constrExisted == triggerExisted || (constrExisted && null dels)
        then migErrs
        else ["Both trigger and constructor table must exist: " ++ cName] ++ migErrs
  return $ (constrExisted, if null errs
    then mergeMigrations $ map showAlterDb $ mig ++ delTrigger ++ updTriggers
    else Left errs)

-- on delete removes all ephemeral data
-- returns column name and delete statement for the referenced table
mkDeletes :: MigrationPack m typ -> [Column DbType] -> [(String, String)]
mkDeletes MigrationPack{..} columns = mapMaybe delField columns where
  delField (Column name _ t _) = fmap delStatement $ ephemeralName t where
    delStatement ref = (name, "DELETE FROM " ++ escape ref ++ " WHERE id=old." ++ escape name ++ ";")
  ephemeralName (DbMaybe x) = ephemeralName x
  ephemeralName (DbList name _) = Just name
  ephemeralName _ = Nothing
