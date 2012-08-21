{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Database.Groundhog.Postgresql.Migration (migrate') where

import Database.Groundhog.Core hiding (Update)
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Postgresql.Base

import Control.Arrow ((&&&), (***), first, second)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Int (Int32)
import Data.List (intercalate, group, groupBy, sort, partition, deleteFirstsBy)
import Data.Maybe (catMaybes, fromJust, fromMaybe, maybeToList)

import Debug.Trace

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
          _ -> liftM snd $ migConstrAndTrigger True name $ head constrs
      else do
        maincolumns <- checkTable name
        let constrTable c = name ++ [defDelim] ++ constrName c
        res <- mapM (\c -> migConstrAndTrigger False name c) constrs
        case maincolumns of
          Nothing -> do
            -- no constructor tables can exist if there is no main data table
            let orphans = filter fst res
            return $ if null orphans
              then mergeMigrations $ Right [(False, defaultPriority, mainTableQuery)]:map snd res
              else Left $ foldl (\l (_, c) -> ("Orphan constructor table found: " ++ constrTable c):l) [] $ filter (fst.fst) $ zip res constrs
          Just (Right (Just mainTableId, columns, [], [])) -> do
            if compareColumns columns mainTableColumns
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
    let valuesName = mainName ++ "$" ++ "values"
    let (valueCols, valueRefs) = mkColumns "value" t
    let mainQuery = "CREATE TABLE " ++ escape mainName ++ " (id SERIAL PRIMARY KEY UNIQUE)"
    let items = ("id INTEGER NOT NULL REFERENCES " ++ escape mainName ++ " ON DELETE CASCADE"):"ord INTEGER NOT NULL" : map showColumn valueCols
    let valuesQuery = "CREATE TABLE " ++ escape valuesName ++ " (" ++ intercalate ", " items ++ ")"
    x <- checkTable mainName
    y <- checkTable valuesName
    let triggerMain = Right []
    (_, triggerValues) <- migTriggerOnDelete valuesName $ map snd $ mkDeletes valueCols
    let addReferences = map (createReference valuesName) valueRefs
    return $ case (x, y) of
      (Nothing, Nothing) -> mergeMigrations $ [Right [(False, defaultPriority, mainQuery), (False, defaultPriority, valuesQuery)]] ++ addReferences ++ [triggerMain, triggerValues]
      (Just (Right mainStructure), Just (Right valuesStructure)) -> let
        f name a@(id1, cols1, uniqs1, refs1) b@(id2, cols2, uniqs2, refs2) = if id1 == id2 && compareColumns cols1 cols2 && haveSameElems compareUniqs uniqs1 uniqs2 && haveSameElems compareRefs refs1 refs2
          then []
          else ["List table " ++ name ++ " error. Expected: " ++ show a ++ ". Found: " ++ show b]
        expectedMainStructure = (Just "id", [], [], [])
        expectedValuesStructure = (Nothing, Column "id" False (dbType (0 :: Int32)) Nothing : Column "ord" False (dbType (0 :: Int32)) Nothing : valueCols, [], map (\x -> (Nothing, x)) $ (mainName, [("id", "id")]) : valueRefs)
        errors = f mainName expectedMainStructure mainStructure ++ f valuesName expectedValuesStructure valuesStructure
        in if null errors then Right [] else Left errors
      (Just (Left errs1), Just (Left errs2)) -> Left $ errs1 ++ errs2
      (Just (Left errs), Just _) -> Left errs
      (Just _, Just (Left errs)) -> Left errs
      (_, Nothing) -> Left ["Found orphan main list table " ++ mainName]
      (Nothing, _) -> Left ["Found orphan list values table " ++ valuesName]
  migL t = fail $ "migrate: expected DbList, got " ++ show t

createReference :: String -> Reference -> SingleMigration
createReference tname ref = Right [(False, referencePriority, showAlterTable tname (AddReference ref))]
  
showColumn :: Column -> String
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

migConstrAndTrigger :: (MonadBaseControl IO m, MonadIO m) => Bool -> String -> ConstructorDef -> DbPersist Postgresql m (Bool, SingleMigration)
migConstrAndTrigger simple name constr = do
  let cName = if simple then name else name ++ [defDelim] ++ constrName constr
  (constrExisted, mig) <- migConstr (if simple then Nothing else Just name) cName constr
  let columns = concatMap (fst . uncurry mkColumns) $ constrParams constr
  let dels = mkDeletes columns
  (triggerExisted, delTrigger) <- migTriggerOnDelete cName (map snd dels)
  updTriggers <- mapM (liftM snd . uncurry (migTriggerOnUpdate cName)) dels
  return (constrExisted, mig)
{-  return $ if constrExisted == triggerExisted || (constrExisted && null dels)
    then (constrExisted, mergeMigrations ([mig, delTrigger] ++ updTriggers))
    -- this can happen when an ephemeral field was added. Consider doing something else except throwing an error
    else (constrExisted, Left ["Both trigger and constructor table must exist: " ++ cName])
-}

migConstr :: (MonadBaseControl IO m, MonadIO m) => Maybe String -> String -> ConstructorDef -> DbPersist Postgresql m (Bool, SingleMigration)
migConstr mainTableName cName constr = do
  let (columns, refs) = concat *** concat $ unzip $ map (uncurry mkColumns) $ constrParams constr
  let uniques = constrUniques constr
  let mainRef = maybe "" (\x -> " REFERENCES " ++ escape x ++ " ON DELETE CASCADE ") mainTableName
  let autoKey = fmap (\x -> escape x ++ " SERIAL PRIMARY KEY UNIQUE" ++ mainRef) $ constrAutoKeyName constr
  let items = maybeToList autoKey ++ map showColumn columns
  let addTable = "CREATE TABLE " ++ escape cName ++ " (" ++ intercalate "," items ++ ")"
  let addReferences = map (createReference cName) refs
  x <- checkTable cName
  case x of
    Just (Right old) -> do
      liftIO $ print $ "old: " ++ show old
      liftIO $ print $ "new: " ++ show ((constrAutoKeyName constr, columns, uniques, map (\r -> (Nothing, first escape r)) refs) :: TableInfo)
    _ -> return ()
  return $ case x of
    Nothing  -> let
      rest = map (\(UniqueDef name fields) -> AlterTable cName $ AddUniqueConstraint name (map fst fields)) uniques
      in (False, mergeMigrations $ (Right $ (map showAlterDb $ (AddTable addTable):rest)) : addReferences)
    Just (Right old) -> let
      refs' = map (\r -> (Nothing, r)) $ maybeToList (fmap (\x -> (x, [(fromJust $ constrAutoKeyName constr, mainTableId)])) mainTableName) ++ refs
      (acs, ats) = getAlters old (constrAutoKeyName constr, columns, uniques, refs')
      acs' = map (AlterColumn cName) acs
      ats' = map (AlterTable cName) ats
      in (True, Right $ map showAlterDb $ acs' ++ ats')
    Just (Left errs) -> (True, Left errs)

-- it handles only delete operations. So far when list or tuple replace is not allowed, it is ok
migTriggerOnDelete :: MonadBaseControl IO m => String -> [String] -> DbPersist Postgresql m (Bool, SingleMigration)
migTriggerOnDelete name deletes = return (False, Right [])
      
-- | Table name and a  list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: MonadBaseControl IO m => String -> String -> String -> DbPersist Postgresql m (Bool, SingleMigration)
migTriggerOnUpdate name fieldName del = return (False, Right [])
{-
migTriggerOnUpdate name fieldName del = do
  let tname = name ++ "$" ++ fieldName
  let query = "CREATE TRIGGER " ++ escape tname ++ " UPDATE OF " ++ escape fieldName ++ " ON " ++ escape name ++ " BEGIN " ++ del ++ "END"
  x <- checkTrigger tname
  return $ case x of
    Nothing -> (False, Right [(False, query)])
    Just sql -> (True, if sql == query
        then Right []
        else Left ["The trigger " ++ tname ++ " is different from expected. Manual migration required.\n" ++ sql ++ "\n" ++ query])
-}

-- on delete removes all ephemeral data
-- returns column name and delete statement for the referenced table
mkDeletes :: [Column] -> [(String, String)]
mkDeletes columns = catMaybes $ map delField columns where
  delField (Column name _ t _) = fmap delStatement $ ephemeralName t where
    delStatement ref = (name, "DELETE FROM " ++ ref ++ " WHERE id=old." ++ escape name ++ ";")
  ephemeralName (DbMaybe x) = ephemeralName x
  ephemeralName (DbList name _) = Just name
  ephemeralName _ = Nothing

-- | primary key name, columns, uniques, and references wtih constraint names
type TableInfo = (Maybe String, [Column], [UniqueDef], [(Maybe String, Reference)])

checkTable :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m (Maybe (Either [String] TableInfo))
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

getColumn :: String -> (String, String, String, Maybe String) -> Either String Column
getColumn tname (column_name, is_nullable, udt_name, d) = case readSqlType udt_name of
      Left s -> Left s
      Right t -> Right $ Column column_name (is_nullable == "YES") t d

checkTableReferences :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m [(Maybe String, Reference)]
checkTableReferences tableName = do
  let sql = "SELECT c.conname, c.foreign_table, a_child.attname AS child, a_parent.attname AS parent FROM (SELECT r.confrelid::regclass AS foreign_table, r.conrelid, r.confrelid, unnest(r.conkey) AS conkey, unnest(r.confkey) AS confkey, r.conname FROM pg_catalog.pg_constraint r WHERE r.conrelid = ?::regclass AND r.contype = 'f') AS c INNER JOIN pg_attribute a_parent ON a_parent.attnum = c.confkey AND a_parent.attrelid = c.confrelid INNER JOIN pg_attribute a_child ON a_child.attnum = c.conkey AND a_child.attrelid = c.conrelid ORDER BY c.conname"
  x <- queryRaw' sql [toPrim proxy $ escape tableName] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  -- (refName, (parentTable, (childColumn, parentColumn)))
  let mkReference xs = (Just refName, (parentTable, map (snd . snd) xs)) where
        (refName, (parentTable, _)) = head xs
      references = map mkReference $ groupBy ((==) `on` fst) x
  return references

data AlterColumn = Type DbType | IsNull | NotNull | Add Column | Drop | AddPrimaryKey
                 | Default String | NoDefault | Update String
type AlterColumn' = (String, AlterColumn)

data AlterTable = AddUniqueConstraint String [String]
                | DropConstraint String
                | AddReference Reference
                | DropReference String

data AlterDB = AddTable String
             | AlterColumn String AlterColumn'
             | AlterTable String AlterTable

-- from database, from datatype
getAlters :: TableInfo
          -> TableInfo
          -> ([AlterColumn'], [AlterTable])
getAlters (oldId, oldColumns, oldUniques, oldRefs) (newId, newColumns, newUniques, newRefs) = (colAlters, tableAlters)
  where
    (oldOnlyColumns, newOnlyColumns, matchedColumns) = matchElements (\old new -> cName old == cName new) oldColumns newColumns
    (oldOnlyUniques, newOnlyUniques, matchedUniques) = matchElements compareUniqs oldUniques newUniques
    primaryKeyAlters = case (oldId, newId) of
      (Nothing, Just newName) -> [(newName, AddPrimaryKey)]
      (Just oldName, Nothing) -> [(oldName, Drop)]
      (Just oldName, Just newName) | oldName /= newName -> error $ "getAlters: cannot rename primary key (old " ++ oldName ++ ", new " ++ newName ++ ")"
      _ -> []
    (oldOnlyRefs, newOnlyRefs, _) = (\x -> trace (show x) x) $ matchElements compareRefs oldRefs newRefs

    colAlters = map (\x -> (cName x, Drop)) oldOnlyColumns ++ map (\x -> (cName x, Add x)) newOnlyColumns ++ concatMap migrateColumn matchedColumns ++ primaryKeyAlters
    tableAlters = 
         map (\(UniqueDef name _) -> DropConstraint name) oldOnlyUniques
      ++ map (\(UniqueDef name cols) -> AddUniqueConstraint name (map fst cols)) newOnlyUniques
      ++ concatMap migrateUniq matchedUniques
      ++ map (DropReference . fromMaybe (error "getAlters: old reference does not have name") . fst) oldOnlyRefs
      ++ map (AddReference . snd) newOnlyRefs
    

-- from database, from datatype
migrateUniq :: (UniqueDef, UniqueDef) -> [AlterTable]
migrateUniq (UniqueDef name cols, UniqueDef name' cols') = if sort (map fst cols) == sort (map fst cols')
  then []
  else [DropConstraint name, AddUniqueConstraint name' $ map fst cols']

-- from database, from datatype
migrateColumn :: (Column, Column) -> [AlterColumn']
migrateColumn (Column name isNull type_ def, Column _ isNull' type_' def') = modDef ++ modNull ++ modType where
  modNull = case (isNull, isNull') of
    (False, True) -> [(name, IsNull)]
    (True, False) -> case def' of
      Nothing -> [(name, NotNull)]
      Just s -> [(name, Update s), (name, NotNull)]
    _ -> []
  modType = if simplifyType type_ == simplifyType type_' then [] else [(name, Type type_')]
  modDef = if def == def'
    then []
    else [(name, maybe NoDefault Default def')]

showAlterDb :: AlterDB -> (Bool, Int, String)
showAlterDb (AddTable s) = (False, defaultPriority, s)
showAlterDb (AlterColumn t (c, ac)) =
    (isUnsafe ac, defaultPriority, showAlter t (c, ac))
  where
    isUnsafe Drop = True
    isUnsafe _ = False
showAlterDb (AlterTable t at) = (False, priority, showAlterTable t at) where
  priority = case at of
    AddReference _ -> referencePriority
    _              -> defaultPriority

showAlterTable :: String -> AlterTable -> String
showAlterTable table (AddUniqueConstraint cname cols) = concat
    [ "ALTER TABLE "
    , escape table
    , " ADD CONSTRAINT "
    , escape cname
    , " UNIQUE("
    , intercalate "," $ map escape cols
    , ")"
    ]
showAlterTable table (DropConstraint cname) = concat
    [ "ALTER TABLE "
    , escape table
    , " DROP CONSTRAINT "
    , escape cname
    ]
showAlterTable table (AddReference (tName, columns)) = concat
    [ "ALTER TABLE "
    , escape table
    , " ADD FOREIGN KEY("
    , our
    , ") REFERENCES "
    , escape tName
    , "("
    , foreign
    , ")"
    ] where
    (our, foreign) = f *** f $ unzip columns
    f = intercalate ", " . map escape
showAlterTable table (DropReference name) =
    "ALTER TABLE " ++ escape table ++ " DROP CONSTRAINT " ++ name

showAlter :: String -> AlterColumn' -> String
showAlter table (n, Type t) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " TYPE "
        , showSqlType t
        ]
showAlter table (n, IsNull) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " DROP NOT NULL"
        ]
showAlter table (n, NotNull) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " SET NOT NULL"
        ]
showAlter table (_, Add col) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ADD COLUMN "
        , showColumn col
        ]
showAlter table (n, Drop) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " DROP COLUMN "
        , escape n
        ]
showAlter table (n, AddPrimaryKey) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ADD COLUMN "
        , escape n
        , " SERIAL PRIMARY KEY UNIQUE"
        ]
showAlter table (n, Default s) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " SET DEFAULT "
        , s
        ]
showAlter table (n, NoDefault) = concat
    [ "ALTER TABLE "
    , escape table
    , " ALTER COLUMN "
    , escape n
    , " DROP DEFAULT"
    ]
showAlter table (n, Update s) = concat
    [ "UPDATE "
    , escape table
    , " SET "
    , escape n
    , "="
    , s
    , " WHERE "
    , escape n
    , " IS NULL"
    ]
    
-- TODO: move all code below to generic modules

readSqlType :: String -> Either String DbType
readSqlType "int4" = Right $ DbInt32
readSqlType "int8" = Right $ DbInt64
readSqlType "varchar" = Right $ DbString
readSqlType "date" = Right $ DbDay
readSqlType "bool" = Right $ DbBool
readSqlType "timestamp" = Right $ DbDayTime
readSqlType "float4" = Right $ DbReal
readSqlType "float8" = Right $ DbReal
readSqlType "bytea" = Right $ DbBlob
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
showSqlType DbBlob = "BYTEA"
showSqlType (DbMaybe t) = showSqlType t
showSqlType (DbList _ _) = "INTEGER"
showSqlType (DbEntity Nothing _) = "INTEGER"
showSqlType t = error $ "showSqlType: DbType does not have corresponding database type: " ++ show t

compareColumns :: [Column] -> [Column] -> Bool
compareColumns = haveSameElems ((==) `on` f) where
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

mainTableId :: String
mainTableId = "id"
