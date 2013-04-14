{-# LANGUAGE RecordWildCards #-}
-- | This helper module is intended for use by the backend creators
module Database.Groundhog.Generic.Migration
  ( Column(..)
  , UniqueDef'(..)
  , Reference(..)
  , ReferenceAction(..)
  , TableInfo(..)
  , AlterColumn(..)
  , AlterTable(..)
  , AlterDB(..)
  , MigrationPack(..)
  , SchemaAnalyzer(..)
  , mkColumns
  , migrateRecursively
  , migrateEntity
  , migrateList
  , getAlters
  , defaultMigConstr
  , showReferenceAction
  , readReferenceAction
  ) where

import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql (tableName)

import Control.Arrow ((***), (&&&))
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), gets, modify)
import Data.Function (on)
import qualified Data.Map as Map
import Data.List (group, intercalate)
import Data.Maybe (fromJust, fromMaybe, mapMaybe, maybeToList)

data Column = Column
    { colName :: String
    , colNull :: Bool
    , colType :: DbType -- ^ contains DbType that maps to one column (no DbEmbedded)
    , colDefault :: Maybe String
    } deriving (Eq, Show)

data Reference = Reference {
    referencedTableSchema :: Maybe String
  , referencedTableName :: String
  , referencedColumns :: [(String, String)] -- ^ child column, parent column
  , referenceOnDelete :: Maybe ReferenceAction
  , referenceOnUpdate :: Maybe ReferenceAction
  } deriving Show

data ReferenceAction = NoAction
                     | Restrict
                     | Cascade
                     | SetNull
                     | SetDefault
  deriving (Eq, Show)

data TableInfo = TableInfo {
    tableColumns :: [Column]
  , tableUniques :: [UniqueDef']
    -- | constraint name and reference
  , tableReferences :: [(Maybe String, Reference)]
} deriving Show

data AlterColumn = Type DbType | IsNull | NotNull
                 | Default String | NoDefault | UpdateValue String deriving Show

data AlterTable = AddUnique UniqueDef'
                | DropConstraint String
                | DropIndex String
                | AddReference Reference
                | DropReference String
                | DropColumn String
                | AddColumn Column
                | AlterColumn Column [AlterColumn] deriving Show

data AlterDB = AddTable String
             -- | Table schema, table name, create statement, structure of table from DB, structure of table from datatype, alters
             | AlterTable (Maybe String) String String TableInfo TableInfo [AlterTable]
             -- | Trigger schema, trigger name, table schema, table name
             | DropTrigger (Maybe String) String (Maybe String) String
             -- | Trigger schema, trigger name, table schema, table name, body
             | AddTriggerOnDelete (Maybe String) String (Maybe String) String String
             -- | Trigger schema, trigger name, table schema, table name, field name, body
             | AddTriggerOnUpdate (Maybe String) String (Maybe String) String (Maybe String) String
             -- | Statement which creates the function
             | CreateOrReplaceFunction String
             -- | Function schema, function name
             | DropFunction (Maybe String) String
  deriving Show

data UniqueDef' = UniqueDef' {
    uniqueDefName :: Maybe String
  , uniqueDefType :: UniqueType
  , uniqueDefColumns :: [String]
} deriving Show

data MigrationPack m = MigrationPack {
    compareTypes :: DbType -> DbType -> Bool
  , compareRefs :: (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
  , compareUniqs :: UniqueDef' -> UniqueDef' -> Bool
  , migTriggerOnDelete :: Maybe String -> String -> [(String, String)] -> m (Bool, [AlterDB])
  , migTriggerOnUpdate :: Maybe String -> String -> [(String, String)] -> m [(Bool, [AlterDB])]
  , migConstr :: MigrationPack m -> EntityDef -> ConstructorDef -> m (Bool, SingleMigration)
  , escape :: String -> String
  , primaryKeyType :: DbType
  , primaryKeyTypeName :: String
  , foreignKeyTypeName :: String
  , mainTableId :: String
  , defaultPriority :: Int
  -- | Sql pieces for the create table statement that add constraints and alterations for running after the table is created
  , addUniquesReferences :: [UniqueDef'] -> [Reference] -> ([String], [AlterTable])
  , showColumn :: Column -> String
  , showAlterDb :: AlterDB -> SingleMigration
}

mkColumns :: String -> DbType -> ([Column], [Reference])
mkColumns columnName dbtype = go "" (columnName, dbtype) where
  go prefix (fname, typ) = (case typ of
    DbEmbedded (EmbeddedDef False ts) -> concatMap' (go $ prefix ++ fname ++ [delim]) ts
    DbEmbedded (EmbeddedDef True  ts) -> concatMap' (go "") ts
    DbMaybe a      -> case go prefix (fname, a) of
      ([c], refs) -> ([c {colNull = True}], refs)
      _ -> error $ "mkColumns: datatype inside DbMaybe must be one column " ++ show a
    DbEntity (Just (emb, uName)) e  -> (cols, ref:refs) where
      (cols, refs) = go prefix (fname, DbEmbedded emb)
      ref = Reference (entitySchema e) (entityName e) (zipWith' (curry $ colName *** colName) cols foreignColumns) Nothing Nothing
      cDef = case constructors e of
        [cDef'] -> cDef'
        _       -> error "mkColumns: datatype with unique key cannot have more than one constructor"
      UniqueDef _ _ uFields = findOne "unique" id uniqueName uName $ constrUniques cDef
      fields = map (\(fName, _) -> findOne "field" id fst fName $ constrParams cDef) uFields
      (foreignColumns, _) = concatMap' (go "") fields
    t@(DbEntity Nothing e) -> ([Column name False t Nothing], refs) where
      refs = [Reference (entitySchema e) (entityName e) [(name, keyName)] Nothing Nothing]
      keyName = case constructors e of
        [cDef] -> fromMaybe (error "mkColumns: autokey name is Nothing") $ constrAutoKeyName cDef
        _      -> "id"
    t@(DbList lName _) -> ([Column name False t Nothing], refs) where
      -- TODO: schema
      refs = [Reference Nothing lName [(name, "id")] Nothing Nothing]
    t -> ([Column name False t Nothing], [])) where
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

migrateEntity :: (Monad m, SchemaAnalyzer m) => MigrationPack m -> EntityDef -> m SingleMigration
migrateEntity m@MigrationPack{..} e = do
  let name = entityName e
  let constrs = constructors e
  let mainTableQuery = "CREATE TABLE " ++ escape name ++ " (" ++ mainTableId ++ " " ++ primaryKeyTypeName ++ ", discr INTEGER NOT NULL)"
  let expectedMainStructure = TableInfo [Column "id" False primaryKeyType Nothing, Column "discr" False DbInt32 Nothing] [UniqueDef' Nothing UniquePrimary ["id"]] []

  if isSimple constrs
    then do
      x <- analyzeTable (entitySchema e) name
      -- check whether the table was created for multiple constructors before
      case x of
        Right (Just old) | null $ getAlters m old expectedMainStructure -> do
          return $ Left ["Datatype with multiple constructors was truncated to one constructor. Manual migration required. Datatype: " ++ name]
        Right _ -> liftM snd $ migConstr m e $ head constrs
        Left errs -> return (Left errs)
    else do
      mainStructure <- analyzeTable (entitySchema e) name
      let constrTable c = name ++ [delim] ++ constrName c
      res <- mapM (migConstr m e) constrs
      case mainStructure of
        Right Nothing -> do
          -- no constructor tables can exist if there is no main data table
          let orphans = filter (fst . fst) $ zip res constrs
          return $ if null orphans
            then mergeMigrations $ Right [(False, defaultPriority, mainTableQuery)]:map snd res
            else Left $ map (\(_, c) -> "Orphan constructor table found: " ++ constrTable c) orphans
        Right (Just mainStructure') -> do
          if null $ getAlters m mainStructure' expectedMainStructure
            then do
              -- the datatype had also many constructors before
              -- check whether any new constructors appeared and increment older discriminators, which were shifted by newer constructors inserted not in the end
              let updateDiscriminators = Right . go 0 . map (head &&& length) . group . map fst $ res where
                  go acc ((False, n):(True, n2):xs) = (False, defaultPriority, "UPDATE " ++ escape name ++ " SET discr = discr + " ++ show n ++ " WHERE discr >= " ++ show acc) : go (acc + n + n2) xs
                  go acc ((True, n):xs) = go (acc + n) xs
                  go _ _ = []
              return $ mergeMigrations $ updateDiscriminators: (map snd res)
            else return $ Left ["Unexpected structure of main table for Datatype: " ++ name ++ ". Table info: " ++ show mainStructure']
        Left errs -> return (Left errs)

migrateList :: (Monad m, SchemaAnalyzer m) => MigrationPack m -> DbType -> m SingleMigration
migrateList m@MigrationPack{..} (DbList mainName t) = do
  let valuesName = mainName ++ delim : "values"
      (valueCols, valueRefs) = mkColumns "value" t
      refs' = Reference Nothing mainName [("id", "id")] (Just Cascade) Nothing : valueRefs
      mainQuery = "CREATE TABLE " ++ escape mainName ++ " (id " ++ primaryKeyTypeName ++ ")"
      (addInCreate, addInAlters) = addUniquesReferences [] refs'
      items = ("id " ++ foreignKeyTypeName ++ " NOT NULL"):"ord INTEGER NOT NULL" : map showColumn valueCols ++ addInCreate
      valuesQuery = "CREATE TABLE " ++ escape valuesName ++ " (" ++ intercalate ", " items ++ ")"
      expectedMainStructure = TableInfo [Column "id" False primaryKeyType Nothing] [UniqueDef' Nothing UniquePrimary ["id"]] []
      valueColumns = Column "id" False primaryKeyType Nothing : Column "ord" False DbInt32 Nothing : valueCols
      expectedValuesStructure = TableInfo valueColumns [] (map (\x -> (Nothing, x)) refs')
  -- TODO: handle case when outer entity has a schema
  mainStructure <- analyzeTable Nothing mainName
  valuesStructure <- analyzeTable Nothing valuesName
  let triggerMain = []
  (_, triggerValues) <- migTriggerOnDelete Nothing valuesName $ mkDeletes m valueCols
  return $ case (mainStructure, valuesStructure) of
    (Right Nothing, Right Nothing) -> let
      rest = [AlterTable Nothing valuesName valuesQuery expectedValuesStructure expectedValuesStructure addInAlters]
      in mergeMigrations $ map showAlterDb $ [AddTable mainQuery, AddTable valuesQuery] ++ rest ++ triggerMain ++ triggerValues
    (Right (Just mainStructure'), Right (Just valuesStructure')) -> let
      f name a b = if null $ getAlters m a b
        then []
        else ["List table " ++ name ++ " error. Expected: " ++ show b ++ ". Found: " ++ show a]
      errors = f mainName mainStructure' expectedMainStructure ++ f valuesName valuesStructure' expectedValuesStructure
      in if null errors then Right [] else Left errors
    (Left errs1, Left errs2) -> Left $ errs1 ++ errs2
    (Left errs, Right _) -> Left errs
    (Right _, Left errs) -> Left errs
    (_, Right Nothing) -> Left ["Found orphan main list table " ++ mainName]
    (Right Nothing, _) -> Left ["Found orphan list values table " ++ valuesName]
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
    primaryColumns = concatMap uniqueDefColumns $ filter ((== UniquePrimary) . uniqueDefType) oldUniques

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
  modDef = if def1 == def2
    then []
    else [maybe NoDefault Default def2]

-- from database, from datatype
migrateUniq :: UniqueDef' -> UniqueDef' -> [AlterTable]
migrateUniq u1@(UniqueDef' _ _ cols1) u2@(UniqueDef' _ _ cols2) = if haveSameElems (==) cols1 cols2
  then []
  else [dropUnique u1, AddUnique u2]

dropUnique :: UniqueDef' -> AlterTable
dropUnique (UniqueDef' name typ _) = (case typ of
  UniqueConstraint -> DropConstraint name'
  UniqueIndex -> DropIndex name'
  UniquePrimary -> DropConstraint name') where
  name' = fromMaybe (error $ "dropUnique: constraint which should be dropped does not have a name") name
  
defaultMigConstr :: (Monad m, SchemaAnalyzer m) => MigrationPack m -> EntityDef -> ConstructorDef -> m (Bool, SingleMigration)
defaultMigConstr migPack@MigrationPack{..} e constr = do
  let simple = isSimple $ constructors e
      name = entityName e
      schema = entitySchema e
      cName = if simple then name else name ++ [delim] ++ constrName constr
      mkColumns' xs = concat *** concat $ unzip $ map (uncurry mkColumns) xs
      (columns, refs) = mkColumns' $ constrParams constr
  tableStructure <- analyzeTable schema cName
  let dels = mkDeletes migPack columns
  (triggerExisted, delTrigger) <- migTriggerOnDelete schema cName dels
  updTriggers <- liftM (concatMap snd) $ migTriggerOnUpdate schema cName dels
  
  let (mainRef, columns', refs', uniques') = case constrAutoKeyName constr of
        Nothing -> (primaryKeyTypeName, columns, refs, uniques)
        Just keyName | simple -> (primaryKeyTypeName, Column keyName False primaryKeyType Nothing:columns, refs, uniques ++ [UniqueDef' Nothing UniquePrimary [keyName]])
                     | otherwise -> (foreignKeyTypeName ++ " NOT NULL UNIQUE "
                      , Column (fromJust $ constrAutoKeyName constr) False primaryKeyType Nothing:columns
                      , refs ++ [Reference schema name [(fromJust $ constrAutoKeyName constr, mainTableId)] (Just Cascade) Nothing]
                      , uniques ++ [UniqueDef' Nothing UniqueConstraint [fromJust $ constrAutoKeyName constr]]
                      )

      uniques = map (\(UniqueDef uName uType cols) -> UniqueDef' (Just uName) uType (map colName $ fst $ mkColumns' cols)) $ constrUniques constr

      (addInCreate, addInAlters) = addUniquesReferences uniques refs'
      autoKey = fmap (\x -> escape x ++ " " ++ mainRef) $ constrAutoKeyName constr
      items = maybeToList autoKey ++ map showColumn columns ++ addInCreate
      addTable = "CREATE TABLE " ++ tableName escape e constr ++ " (" ++ intercalate ", " items ++ ")"

-- change primary key and columns depending on isSimple
      expectedTableStructure = TableInfo columns' uniques' (map (\r -> (Nothing, r)) refs')
      (migErrs, constrExisted, mig) = case tableStructure of
        Right Nothing  -> let
          rest = AlterTable schema cName addTable expectedTableStructure expectedTableStructure addInAlters
          in ([], False, [AddTable addTable, rest])
        Right (Just oldTableStructure) -> let
          alters = getAlters migPack oldTableStructure expectedTableStructure
          in ([], True, [AlterTable schema cName addTable oldTableStructure expectedTableStructure alters])
        Left errs -> (errs, True, [])
      -- this can happen when an ephemeral field was added. Consider doing something else except throwing an error
      allErrs = if constrExisted == triggerExisted || (constrExisted && null dels)
        then migErrs
        else ["Both trigger and constructor table must exist: " ++ cName] ++ migErrs
  return $ (constrExisted, if null allErrs
    then mergeMigrations $ map showAlterDb $ mig ++ delTrigger ++ updTriggers
    else Left allErrs)

-- on delete removes all ephemeral data
-- returns column name and delete statement for the referenced table
mkDeletes :: MigrationPack m -> [Column] -> [(String, String)]
mkDeletes MigrationPack{..} columns = mapMaybe delField columns where
  delField (Column name _ t _) = fmap delStatement $ ephemeralName t where
    delStatement ref = (name, "DELETE FROM " ++ escape ref ++ " WHERE id=old." ++ escape name ++ ";")
  ephemeralName (DbMaybe x) = ephemeralName x
  ephemeralName (DbList name _) = Just name
  ephemeralName _ = Nothing

showReferenceAction :: ReferenceAction -> String
showReferenceAction NoAction = "NO ACTION"
showReferenceAction Restrict = "RESTRICT"
showReferenceAction Cascade = "CASCADE"
showReferenceAction SetNull = "SET NULL"
showReferenceAction SetDefault = "SET DEFAULT"

readReferenceAction :: String -> Maybe ReferenceAction
readReferenceAction c = case c of
  "NO ACTION" -> Just NoAction
  "RESTRICT" -> Just Restrict
  "CASCADE" -> Just Cascade
  "SET NULL" -> Just SetNull
  "SET DEFAULT" -> Just SetDefault
  _ -> Nothing

class SchemaAnalyzer m where
  listTables :: Maybe String -- ^ Schema name
             -> m [String]
  listTableTriggers :: Maybe String -- ^ Schema name
                    -> String -- ^ Table name
                    -> m [String]
  analyzeTable :: Maybe String -- ^ Schema name
               -> String -- ^ Table name
               -> m (Either [String] (Maybe TableInfo))
  analyzeTrigger :: Maybe String -- ^ Schema name
                 -> String -- ^ Trigger name
                 -> m (Maybe String)
  analyzeFunction :: Maybe String -- ^ Schema name
                  -> String -- ^ Function name
                  -> m (Maybe String)
