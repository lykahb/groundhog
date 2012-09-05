{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Database.Groundhog.Postgresql.Migration (migrate') where

import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Postgresql.Base

import Control.Arrow ((&&&), (***), second)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Int (Int32)
import Data.List (intercalate, group, groupBy, sort)
import Data.Maybe (mapMaybe, fromJust, fromMaybe, maybeToList)

{- ********************RULES******************** --
For type with a single constructor, a single table is created.
TABLE Entity(id, [fields])
If constructor has no fields, then ????

For type with a multiple constructors, the main table is created.
TABLE(id, discriminator)
where discriminator is defined by constructor.
Each constructor has its table, where id is the same as in 
TABLE EntityConstructor2(id, [fields])

In Java Hibernate each class member of list type is stored in a separate table
TABLE Student$Phones(studentId, phone)
Here we can use triggers to automatically remove list after Student removal.
However, toPersistValue :: a -> DbPersist conn m () becomes impossible because we must know container id

We can either follow this scheme or store same type lists from different types in one table
TABLE List$Int(id, value)

The ephemeral values may exist only if they are referenced to. Eg., a tuple should be removed when a row with its id is removed. Only one reference is allowed.

The triggers are used to delete:
1. Row in the main table when a constructor entry is deleted.
2. Rows in the tables of ephemeral types when a constructor entry is deleted.
3. Rows in the tables of ephemeral types when an ephemeral value row referencing it is deleted. Eg., removing ([Int], Int) row should cause removal of [Int].
4. Rows in the list values table when the entry in the main list is deleted.

-- ********************************************* --}
migrate' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> Migration (DbPersist Postgresql m)
migrate' = migrateRecursively migE migL where
  migE e = do
    let name = entityName e
    let constrs = constructors e
    let mainTableQuery = "CREATE TABLE " ++ escape name ++ " (" ++ mainTableId ++ " SERIAL PRIMARY KEY UNIQUE, discr INT4 NOT NULL)"
    let mainTableColumns = [Column "discr" False DbInt32 Nothing]

    if isSimple constrs
      then do
        x <- checkTable name
        -- check whether the table was created for multiple constructors before
        case x of
          Just (Right (_, columns, _, _)) | columns == mainTableColumns -> do
            return $ Left ["Datatype with multiple constructors was truncated to one constructor. Manual migration required. Datatype: " ++ name]
          Just (Left errs) -> return (Left errs)
          _ -> liftM snd $ migConstr True name $ head constrs
      else do
        maincolumns <- checkTable name
        let constrTable c = name ++ [delim] ++ constrName c
        res <- mapM (\c -> migConstr False name c) constrs
        case maincolumns of
          Nothing -> do
            -- no constructor tables can exist if there is no main data table
            let orphans = filter fst res
            return $ if null orphans
              then mergeMigrations $ Right [(False, defaultPriority, mainTableQuery)]:map snd res
              else Left $ foldl (\l (_, c) -> ("Orphan constructor table found: " ++ constrTable c):l) [] $ filter (fst.fst) $ zip res constrs
          Just (Right (Just _, columns, [], [])) -> do
            if haveSameElems compareColumns columns mainTableColumns
              then do
                -- the datatype had also many constructors before
                -- check whether any new constructors appeared and increment older discriminators, which were shifted by newer constructors inserted not in the end
                let updateDiscriminators = Right . go 0 . map (head &&& length) . group $ map fst $ res where
                    go acc ((False, n):(True, n2):xs) = (False, defaultPriority, "UPDATE " ++ escape name ++ " SET discr = discr + " ++ show n ++ " WHERE discr >= " ++ show acc) : go (acc + n + n2) xs
                    go acc ((True, n):xs) = go (acc + n) xs
                    go _ _ = []
                return $ mergeMigrations $ updateDiscriminators: (map snd res)
              else do
                return $ Left ["Migration from one constructor to many will be implemented soon. Datatype: " ++ name]
          Just (Right structure) -> do
            return $ Left ["Unexpected structure of main table for Datatype: " ++ name ++ ". Table info: " ++ show structure]
          Just (Left errs) -> return (Left errs)
  migL (DbList mainName t) = do
    let valuesName = mainName ++ delim : "values"
    let (valueCols, valueRefs) = mkColumns id "value" t
    let mainQuery = "CREATE TABLE " ++ escape mainName ++ " (id SERIAL PRIMARY KEY UNIQUE)"
    let items = ("id INTEGER NOT NULL REFERENCES " ++ escape mainName ++ " ON DELETE CASCADE"):"ord INTEGER NOT NULL" : map showColumn valueCols
    let valuesQuery = "CREATE TABLE " ++ escape valuesName ++ " (" ++ intercalate ", " items ++ ")"
    let expectedMainStructure = (Just "id", [], [], [])
    let expectedValuesStructure = (Nothing, Column "id" False (dbType (0 :: Int32)) Nothing : Column "ord" False (dbType (0 :: Int32)) Nothing : valueCols, [], map (\x -> (Nothing, x)) $ (mainName, [("id", "id")]) : valueRefs)
    mainStructure <- checkTable mainName
    valuesStructure <- checkTable valuesName
    let triggerMain = []
    (_, triggerValues) <- migTriggerOnDelete valuesName $ mkDeletes valueCols
    return $ case (mainStructure, valuesStructure) of
      (Nothing, Nothing) -> let addReferences = AlterTable valuesName valuesQuery expectedValuesStructure expectedValuesStructure $ map AddReference valueRefs
        in mergeMigrations $ map showAlterDb $ [AddTable mainQuery, AddTable valuesQuery] ++ [addReferences] ++ triggerMain ++ triggerValues
      (Just (Right mainStructure'), Just (Right valuesStructure')) -> let
        f name a@(id1, cols1, uniqs1, refs1) b@(id2, cols2, uniqs2, refs2) = if id1 == id2 && haveSameElems compareColumns cols1 cols2 && haveSameElems compareUniqs uniqs1 uniqs2 && haveSameElems compareRefs refs1 refs2
          then []
          else ["List table " ++ name ++ " error. Expected: " ++ show a ++ ". Found: " ++ show b]
        errors = f mainName expectedMainStructure mainStructure' ++ f valuesName expectedValuesStructure valuesStructure'
        in if null errors then Right [] else Left errors
      (Just (Left errs1), Just (Left errs2)) -> Left $ errs1 ++ errs2
      (Just (Left errs), Just _) -> Left errs
      (Just _, Just (Left errs)) -> Left errs
      (_, Nothing) -> Left ["Found orphan main list table " ++ mainName]
      (Nothing, _) -> Left ["Found orphan list values table " ++ valuesName]
  migL t = fail $ "migrate: expected DbList, got " ++ show t
  
showColumn :: Column DbType -> String
showColumn (Column n nu t def) = concat
    [ escape n
    , " "
    , showSqlType t
    , " "
    , if nu then "NULL" else "NOT NULL"
    , case def of
        Nothing -> ""
        Just s  -> " DEFAULT " ++ s
    ]

migConstr :: (MonadBaseControl IO m, MonadIO m) => Bool -> String -> ConstructorDef -> DbPersist Postgresql m (Bool, SingleMigration)
migConstr simple name constr = do
  let cName = if simple then name else name ++ [delim] ++ constrName constr
  let (columns, refs) = concat *** concat $ unzip $ map (uncurry $ mkColumns id) $ constrParams constr
  tableStructure <- checkTable cName
  let dels = mkDeletes columns
  (triggerExisted, delTrigger) <- migTriggerOnDelete cName dels
  updTriggers <- liftM concat $ mapM (liftM snd . uncurry (migTriggerOnUpdate cName)) dels
  
  let mainTableName = if simple then Nothing else Just name
      refs' = maybeToList (fmap (\x -> (x, [(fromJust $ constrAutoKeyName constr, mainTableId)])) mainTableName) ++ refs

      mainRef = maybe "" (\x -> " REFERENCES " ++ escape x ++ " ON DELETE CASCADE ") mainTableName
      autoKey = fmap (\x -> escape x ++ " SERIAL PRIMARY KEY UNIQUE" ++ mainRef) $ constrAutoKeyName constr
  
      uniques = constrUniques constr
      -- refs instead of refs' because the reference to the main table id is hardcoded in mainRef
      items = maybeToList autoKey ++ map showColumn columns
      addTable = "CREATE TABLE " ++ escape cName ++ " (" ++ intercalate ", " items ++ ")"

      expectedTableStructure = (constrAutoKeyName constr, columns, uniques, map (\r -> (Nothing, r)) refs')
      (migErrs, constrExisted, mig) = case tableStructure of
        Nothing  -> let
          rest = AlterTable cName addTable expectedTableStructure expectedTableStructure $ map (\(UniqueDef uName fields) -> AddUniqueConstraint uName (map fst fields)) uniques ++ map AddReference refs
          in ([], False, [AddTable addTable, rest])
        Just (Right oldTableStructure) -> let
          alters = getAlters oldTableStructure expectedTableStructure
          in ([], True, [AlterTable cName addTable oldTableStructure expectedTableStructure alters])
        Just (Left x) -> (x, True, [])
      -- this can happen when an ephemeral field was added. Consider doing something else except throwing an error
      errs = if constrExisted == triggerExisted || (constrExisted && null dels)
        then migErrs
        else ["Both trigger and constructor table must exist: " ++ cName] ++ migErrs
  return $ (constrExisted, if null errs
    then mergeMigrations $ map showAlterDb $ mig ++ delTrigger ++ updTriggers
    else Left errs)

{-
test=# select p.proname, p.prosrc from pg_catalog.pg_namespace n inner join pg_catalog.pg_proc p on p.pronamespace = n.oid where n.nspname = 'public';
   proname   |          prosrc          
-------------+--------------------------
 delete_tbl1 |  begin return a+b; end;
-}

checkFunction :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m (Maybe String)
checkFunction name = do
  x <- queryRaw' "SELECT p.prosrc FROM pg_catalog.pg_namespace n INNER JOIN pg_catalog.pg_proc p ON p.pronamespace = n.oid WHERE n.nspname = 'public' AND p.proname = ?" [toPrim proxy name] id
  case x of
    Nothing  -> return Nothing
    Just src -> return (fst $ fromPurePersistValues proxy src)

checkTrigger :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m (Maybe String)
checkTrigger name = do
  x <- queryRaw' "SELECT action_statement FROM information_schema.triggers WHERE trigger_name = ?" [toPrim proxy name] id
  case x of
    Nothing  -> return Nothing
    Just src -> return (fst $ fromPurePersistValues proxy src)

-- it handles only delete operations. So far when list or tuple replace is not allowed, it is ok
migTriggerOnDelete :: (MonadBaseControl IO m, MonadIO m) => String -> [(String, String)] -> DbPersist Postgresql m (Bool, [AlterDB DbType])
migTriggerOnDelete name deletes = do
  let funcName = name
  let trigName = name
  func <- checkFunction funcName
  trig <- checkTrigger trigName
  let funcBody = "BEGIN " ++ concatMap snd deletes ++ "RETURN NEW;END;"
      addFunction = CreateOrReplaceFunction $ "CREATE OR REPLACE FUNCTION " ++ escape funcName ++ "() RETURNS trigger AS $$" ++ funcBody ++ "$$ LANGUAGE plpgsql"
      funcMig = case func of
        Nothing | null deletes -> []
        Nothing   -> [addFunction]
        Just body -> if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
          then [DropFunction funcName]
          else if body == funcBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropFunction funcName, addFunction]

      trigBody = "EXECUTE PROCEDURE " ++ escape funcName ++ "()"
      addTrigger = AddTriggerOnDelete trigName name trigBody
      (trigExisted, trigMig) = case trig of
        Nothing | null deletes -> (False, [])
        Nothing   -> (False, [addTrigger])
        Just body -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
          then [DropTrigger trigName name]
          else if body == trigBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropTrigger trigName name, addTrigger])
  return (trigExisted, funcMig ++ trigMig)
  
      
-- | Table name and a  list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: (MonadBaseControl IO m, MonadIO m) => String -> String -> String -> DbPersist Postgresql m (Bool, [AlterDB DbType])
migTriggerOnUpdate name fieldName del = do
  let funcName = name ++ delim : fieldName
  let trigName = name ++ delim : fieldName
  func <- checkFunction funcName
  trig <- checkTrigger trigName
  let funcBody = "BEGIN " ++ del ++ "RETURN NEW;END;"
      addFunction = CreateOrReplaceFunction $ "CREATE OR REPLACE FUNCTION " ++ escape funcName ++ "() RETURNS trigger AS $$" ++ funcBody ++ "$$ LANGUAGE plpgsql"
      funcMig = case func of
        Nothing   -> [addFunction]
        Just body -> if body == funcBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropFunction funcName, addFunction]

      trigBody = "EXECUTE PROCEDURE " ++ escape funcName ++ "()"
      addTrigger = AddTriggerOnUpdate trigName name fieldName trigBody
      (trigExisted, trigMig) = case trig of
        Nothing   -> (False, [addTrigger])
        Just body -> (True, if body == trigBody
            then []
            -- this can happen when an ephemeral field was added or removed.
            else [DropTrigger trigName name, addTrigger])
  return (trigExisted, funcMig ++ trigMig)

-- on delete removes all ephemeral data
-- returns column name and delete statement for the referenced table
{-mkDeletes :: [Column DbType] -> [(String, String)]
mkDeletes columns = zipWith delStatement [0..] $ mapMaybe f columns where
  f col = ephemeralName (cType col) >>= \ephName -> return (col, ephName)
  delStatement :: Int -> (Column DbType, String) -> (String, String)
  delStatement i (col, ref) = (cName col, "IF TG_ARGV[" ++ show i ++ "] IS NOT NULL THEN DELETE FROM " ++ escape ref ++ " WHERE id=TG_ARGV[" ++ show i ++ "]; END IF;")
  ephemeralName (DbMaybe x) = ephemeralName x
  ephemeralName (DbList name _) = Just name
  ephemeralName _ = Nothing -}
  
mkDeletes :: [Column DbType] -> [(String, String)]
mkDeletes columns = mapMaybe delField columns where
  delField (Column name _ t _) = fmap delStatement $ ephemeralName t where
    delStatement ref = (name, "DELETE FROM " ++ escape ref ++ " WHERE id=old." ++ escape name ++ ";")
  ephemeralName (DbMaybe x) = ephemeralName x
  ephemeralName (DbList name _) = Just name
  ephemeralName _ = Nothing

-- | primary key name, columns, uniques, and references wtih constraint names
type TableInfo typ = (Maybe String, [Column typ], [UniqueDef], [(Maybe String, Reference)])

checkTable :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m (Maybe (Either [String] (TableInfo DbType)))
checkTable name = do
  table <- queryRaw' "SELECT * FROM information_schema.tables WHERE table_name=?" [toPrim proxy name] id
  case table of
    Just _ -> do
      -- omit primary keys
      cols <- queryRaw' "SELECT c.column_name, c.is_nullable, c.udt_name, c.column_default FROM information_schema.columns c WHERE c.table_name=? AND c.column_name NOT IN (SELECT c.column_name FROM information_schema.table_constraints tc INNER JOIN information_schema.constraint_column_usage u ON tc.constraint_catalog = u.constraint_catalog AND tc.constraint_schema=u.constraint_schema AND tc.constraint_name=u.constraint_name INNER JOIN information_schema.columns c ON u.table_catalog=c.table_catalog AND u.table_schema=c.table_schema AND u.table_name=c.table_name AND u.column_name=c.column_name WHERE tc.constraint_type='PRIMARY KEY' AND tc.table_name=?) ORDER BY c.ordinal_position" [toPrim proxy name, toPrim proxy name] (mapAllRows $ return . getColumn name . fst . fromPurePersistValues proxy)
      let (col_errs, cols') = partitionEithers cols
      
      let helperU (con, (col, typ)) = case readSqlType typ of
            Left s -> Left s
            Right typ' -> Right (con, (col, typ'))
      rawUniqs <- queryRaw' "SELECT u.constraint_name, u.column_name, c.udt_name FROM information_schema.table_constraints tc INNER JOIN information_schema.constraint_column_usage u ON tc.constraint_catalog=u.constraint_catalog AND tc.constraint_schema=u.constraint_schema AND tc.constraint_name=u.constraint_name INNER JOIN information_schema.columns c ON u.table_catalog=c.table_catalog AND u.table_schema=c.table_schema AND u.table_name=c.table_name AND u.column_name=c.column_name WHERE u.table_name=? AND tc.constraint_type='UNIQUE' ORDER BY u.constraint_name, u.column_name" [toPrim proxy name] (mapAllRows $ return . helperU . fst . fromPurePersistValues proxy)
      let (uniq_errs, uniqRows) = partitionEithers rawUniqs
      let mkUniq us = UniqueDef (fst $ head us) (map snd us)
      let uniqs' = map mkUniq $ groupBy ((==) `on` fst) uniqRows
      references <- checkTableReferences name
      primaryKeyResult <- checkPrimaryKey name
      let (primary_errs, primaryKey, uniqs'') = case primaryKeyResult of
            Left errs -> (errs, Nothing, uniqs')
            Right (Left primaryKeyName) -> ([], primaryKeyName, uniqs')
            Right (Right u) -> ([], Nothing, u:uniqs')
      return $ Just $ case col_errs ++ uniq_errs ++ primary_errs of
        []   -> Right (primaryKey, cols', uniqs'', references)
        errs -> Left errs
    Nothing -> return Nothing

checkPrimaryKey :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m (Either [String] (Either (Maybe String) UniqueDef))
checkPrimaryKey name = do
  let helperU (con, (col, typ)) = case readSqlType typ of
        Left s -> Left s
        Right typ' -> Right (con, (col, typ'))
  rawUniqs <- queryRaw' "SELECT u.constraint_name, u.column_name, c.udt_name FROM information_schema.table_constraints tc INNER JOIN information_schema.constraint_column_usage u ON tc.constraint_catalog = u.constraint_catalog AND tc.constraint_schema=u.constraint_schema AND tc.constraint_name=u.constraint_name INNER JOIN information_schema.columns c ON u.table_catalog=c.table_catalog AND u.table_schema=c.table_schema AND u.table_name=c.table_name AND u.column_name=c.column_name WHERE tc.constraint_type='PRIMARY KEY' AND tc.table_name=?" [toPrim proxy name] (mapAllRows $ return . helperU . fst . fromPurePersistValues proxy)
  let (uniq_errs, uniqRows) = partitionEithers rawUniqs
  let mkUniq us = UniqueDef (fst $ head us) (map snd us)
  return $ case uniq_errs of
    [] -> Right $ case uniqRows of
      [] -> Left Nothing
      [(_, (primaryKeyName, _))] -> Left $ Just primaryKeyName
      us -> Right $ mkUniq us
    errs -> Left errs

getColumn :: String -> (String, String, String, Maybe String) -> Either String (Column DbType)
getColumn tname (column_name, is_nullable, udt_name, d) = case readSqlType udt_name of
      Left s -> Left s
      Right t -> Right $ Column column_name (is_nullable == "YES") t d

checkTableReferences :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m [(Maybe String, Reference)]
checkTableReferences tableName = do
  let sql = "SELECT c.conname, c.foreign_table || '', a_child.attname AS child, a_parent.attname AS parent FROM (SELECT r.confrelid::regclass AS foreign_table, r.conrelid, r.confrelid, unnest(r.conkey) AS conkey, unnest(r.confkey) AS confkey, r.conname FROM pg_catalog.pg_constraint r WHERE r.conrelid = ?::regclass AND r.contype = 'f') AS c INNER JOIN pg_attribute a_parent ON a_parent.attnum = c.confkey AND a_parent.attrelid = c.confrelid INNER JOIN pg_attribute a_child ON a_child.attnum = c.conkey AND a_child.attrelid = c.conrelid ORDER BY c.conname"
  x <- queryRaw' sql [toPrim proxy $ escape tableName] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  -- (refName, (parentTable, (childColumn, parentColumn)))
  let mkReference xs = (Just refName, (parentTable, map (snd . snd) xs)) where
        (refName, (parentTable, _)) = head xs
      references = map mkReference $ groupBy ((==) `on` fst) x
  return references

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

-- from database, from datatype
getAlters :: TableInfo DbType
          -> TableInfo DbType
          -> [AlterTable]
getAlters (oldId, oldColumns, oldUniques, oldRefs) (newId, newColumns, newUniques, newRefs) = map AlterColumn colAlters ++ tableAlters
  where
    (oldOnlyColumns, newOnlyColumns, commonColumns) = matchElements compareColumns oldColumns newColumns
    (oldOnlyUniques, newOnlyUniques, commonUniques) = matchElements compareUniqs oldUniques newUniques
    primaryKeyAlters = case (oldId, newId) of
      (Nothing, Just newName) -> [(newName, AddPrimaryKey)]
      (Just oldName, Nothing) -> [(oldName, Drop)]
      (Just oldName, Just newName) | oldName /= newName -> error $ "getAlters: cannot rename primary key (old " ++ oldName ++ ", new " ++ newName ++ ")"
      _ -> []
    (oldOnlyRefs, newOnlyRefs, _) = matchElements compareRefs oldRefs newRefs

    colAlters = map (\x -> (cName x, Drop)) oldOnlyColumns ++ map (\x -> (cName x, Add x)) newOnlyColumns ++ concatMap migrateColumn commonColumns ++ primaryKeyAlters
    tableAlters = 
         map (\(UniqueDef name _) -> DropConstraint name) oldOnlyUniques
      ++ map (\(UniqueDef name cols) -> AddUniqueConstraint name (map fst cols)) newOnlyUniques
      ++ concatMap migrateUniq commonUniques
      ++ map (DropReference . fromMaybe (error "getAlters: old reference does not have name") . fst) oldOnlyRefs
      ++ map (AddReference . snd) newOnlyRefs
    
-- from database, from datatype
migrateUniq :: (UniqueDef, UniqueDef) -> [AlterTable]
migrateUniq (UniqueDef name cols, UniqueDef name' cols') = if sort (map fst cols) == sort (map fst cols')
  then []
  else [DropConstraint name, AddUniqueConstraint name' $ map fst cols']

-- from database, from datatype
migrateColumn :: (Column DbType, Column DbType) -> [AlterColumn']
migrateColumn (Column name isNull type_ def, Column _ isNull' type_' def') = modDef ++ modNull ++ modType where
  modNull = case (isNull, isNull') of
    (False, True) -> [(name, IsNull)]
    (True, False) -> case def' of
      Nothing -> [(name, NotNull)]
      Just s -> [(name, UpdateValue s), (name, NotNull)]
    _ -> []
  modType = if simplifyType type_ == simplifyType type_' then [] else [(name, Type type_')]
  modDef = if def == def'
    then []
    else [(name, maybe NoDefault Default def')]

showAlterDb :: AlterDB DbType -> SingleMigration
showAlterDb (AddTable s) = Right [(False, defaultPriority, s)]
showAlterDb (AlterTable t _ _ _ alts) = Right $ map (showAlterTable t) alts
showAlterDb (DropTrigger trigName tName) = Right [(False, triggerPriority, "DROP TRIGGER " ++ escape trigName ++ " ON " ++ escape tName)]
showAlterDb (AddTriggerOnDelete trigName tName body) = Right [(False, triggerPriority, "CREATE TRIGGER " ++ escape trigName ++ " AFTER DELETE ON " ++ escape tName ++ " FOR EACH ROW " ++ body)]
showAlterDb (AddTriggerOnUpdate trigName tName fName body) = Right [(False, triggerPriority, "CREATE TRIGGER " ++ escape trigName ++ " AFTER UPDATE OF " ++ escape fName ++ " ON " ++ escape tName ++ " FOR EACH ROW " ++ body)]
showAlterDb (CreateOrReplaceFunction s) = Right [(False, functionPriority, s)]
showAlterDb (DropFunction funcName) = Right [(False, functionPriority, "DROP FUNCTION " ++ escape funcName ++ "()")]
                 
showAlterTable :: String -> AlterTable -> (Bool, Int, String)
showAlterTable table (AlterColumn alt) = showAlterColumn table alt
showAlterTable table (AddUniqueConstraint cname cols) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " ADD CONSTRAINT "
  , escape cname
  , " UNIQUE("
  , intercalate "," $ map escape cols
  , ")"
  ])
showAlterTable table (DropConstraint cname) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " DROP CONSTRAINT "
  , escape cname
  ])
showAlterTable table (AddReference (tName, columns)) = (False, referencePriority, concat
  [ "ALTER TABLE "
  , escape table
  , " ADD FOREIGN KEY("
  , our
  , ") REFERENCES "
  , escape tName
  , "("
  , foreign
  , ")"
  ]) where
  (our, foreign) = f *** f $ unzip columns
  f = intercalate ", " . map escape
showAlterTable table (DropReference name) = (False, defaultPriority,
    "ALTER TABLE " ++ escape table ++ " DROP CONSTRAINT " ++ name)

showAlterColumn :: String -> AlterColumn' -> (Bool, Int, String)
showAlterColumn table (n, Type t) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " ALTER COLUMN "
  , escape n
  , " TYPE "
  , showSqlType t
  ])
showAlterColumn table (n, IsNull) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " ALTER COLUMN "
  , escape n
  , " DROP NOT NULL"
  ])
showAlterColumn table (n, NotNull) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " ALTER COLUMN "
  , escape n
  , " SET NOT NULL"
  ])
showAlterColumn table (_, Add col) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " ADD COLUMN "
  , showColumn col
  ])
showAlterColumn table (n, Drop) = (True, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " DROP COLUMN "
  , escape n
  ])
showAlterColumn table (n, AddPrimaryKey) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " ADD COLUMN "
  , escape n
  , " SERIAL PRIMARY KEY UNIQUE"
  ])
showAlterColumn table (n, Default s) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " ALTER COLUMN "
  , escape n
  , " SET DEFAULT "
  , s
  ])
showAlterColumn table (n, NoDefault) = (False, defaultPriority, concat
  [ "ALTER TABLE "
  , escape table
  , " ALTER COLUMN "
  , escape n
  , " DROP DEFAULT"
  ])
showAlterColumn table (n, UpdateValue s) = (False, defaultPriority, concat
  [ "UPDATE "
  , escape table
  , " SET "
  , escape n
  , "="
  , s
  , " WHERE "
  , escape n
  , " IS NULL"
  ])
    
-- TODO: move all code below to generic modules

readSqlType :: String -> Either String DbType
readSqlType "int4" = Right $ DbInt32
readSqlType "int8" = Right $ DbInt64
readSqlType "varchar" = Right $ DbString
readSqlType "date" = Right $ DbDay
readSqlType "bool" = Right $ DbBool
readSqlType "timestamp" = Right $ DbDayTime
readSqlType "timestamptz" = Right $ DbDayTimeZoned
readSqlType "float4" = Right $ DbReal
readSqlType "float8" = Right $ DbReal
readSqlType "bytea" = Right $ DbBlob
readSqlType "time" = Right $ DbTime
readSqlType a = Left $ "Unknown type: " ++ a

showSqlType :: DbType -> String
showSqlType DbString = "VARCHAR"
showSqlType DbInt32 = "INT4"
showSqlType DbInt64 = "INT8"
showSqlType DbReal = "DOUBLE PRECISION"
showSqlType DbBool = "BOOLEAN"
showSqlType DbDay = "DATE"
showSqlType DbTime = "TIME"
showSqlType DbDayTime = "TIMESTAMP"
showSqlType DbDayTimeZoned = "TIMESTAMP WITH TIME ZONE"
showSqlType DbBlob = "BYTEA"
showSqlType (DbMaybe t) = showSqlType t
showSqlType (DbList _ _) = "INTEGER"
showSqlType (DbEntity Nothing _) = "INTEGER"
showSqlType t = error $ "showSqlType: DbType does not have corresponding database type: " ++ show t

compareColumns :: Column DbType -> Column DbType -> Bool
compareColumns = ((==) `on` f) where
  f col = col {cType = simplifyType (cType col)}

compareUniqs :: UniqueDef -> UniqueDef -> Bool
compareUniqs (UniqueDef name1 cols1) (UniqueDef name2 cols2) = name1 == name2 && haveSameElems ((==) `on` second simplifyType) cols1 cols2

compareRefs :: (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
compareRefs (_, (tbl1, pairs1)) (_, (tbl2, pairs2)) = unescape tbl1 == unescape tbl2 && haveSameElems (==) pairs1 pairs2 where
  unescape name = if head name == '"' && last name == '"' then tail $ init name else name

-- | Converts complex datatypes that reference other data to id type DbInt32. Does not handle DbTuple
simplifyType :: DbType -> DbType
simplifyType (DbEntity Nothing _) = DbInt32
simplifyType (DbList _ _) = DbInt32
simplifyType x = x

defaultPriority :: Int
defaultPriority = 0

referencePriority :: Int
referencePriority = 1

functionPriority :: Int
functionPriority = 2

triggerPriority :: Int
triggerPriority = 3

mainTableId :: String
mainTableId = "id"
