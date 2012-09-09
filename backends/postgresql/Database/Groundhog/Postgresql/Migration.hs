{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}
module Database.Groundhog.Postgresql.Migration (migrate') where

import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Migration hiding (MigrationPack(..))
import qualified Database.Groundhog.Generic.Migration as GM
import Database.Groundhog.Postgresql.Base

import Control.Arrow ((***))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (intercalate, groupBy)

migrate' :: (PersistEntity v, MonadBaseControl IO m, MonadIO m) => v -> Migration (DbPersist Postgresql m)
migrate' = migrateRecursively (migrateEntity migrationPack) (migrateList migrationPack)

migrationPack :: (MonadBaseControl IO m, MonadIO m) => GM.MigrationPack (DbPersist Postgresql m) DbType
migrationPack = GM.MigrationPack
  compareColumns
  compareRefs
  compareUniqs
  checkTable
  migTriggerOnDelete
  migTriggerOnUpdate
  GM.defaultMigConstr
  escape
  "SERIAL PRIMARY KEY UNIQUE"
  "INT8"
  mainTableId
  defaultPriority
  simplifyType
  (\uniques refs -> ([], map (\(UniqueDef' uName fields) -> AddUniqueConstraint uName fields) uniques ++ map AddReference refs))
  showColumn
  showAlterDb

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

{-
test=# select p.proname, p.prosrc from pg_catalog.pg_namespace n inner join pg_catalog.pg_proc p on p.pronamespace = n.oid where n.nspname = 'public';
   proname   |          prosrc          
-------------+--------------------------
 delete_tbl1 |  begin return a+b; end;
-}

checkFunction :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m (Maybe String)
checkFunction name = do
  x <- queryRaw' "SELECT p.prosrc FROM pg_catalog.pg_namespace n INNER JOIN pg_catalog.pg_proc p ON p.pronamespace = n.oid WHERE n.nspname = 'public' AND p.proname = ?" [toPrimitivePersistValue proxy name] id
  case x of
    Nothing  -> return Nothing
    Just src -> return (fst $ fromPurePersistValues proxy src)

checkTrigger :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m (Maybe String)
checkTrigger name = do
  x <- queryRaw' "SELECT action_statement FROM information_schema.triggers WHERE trigger_name = ?" [toPrimitivePersistValue proxy name] id
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
  
checkTable :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m (Maybe (Either [String] (TableInfo DbType)))
checkTable name = do
  table <- queryRaw' "SELECT * FROM information_schema.tables WHERE table_name=?" [toPrimitivePersistValue proxy name] id
  case table of
    Just _ -> do
      -- omit primary keys
      cols <- queryRaw' "SELECT c.column_name, c.is_nullable, c.udt_name, c.column_default FROM information_schema.columns c WHERE c.table_name=? AND c.column_name NOT IN (SELECT c.column_name FROM information_schema.table_constraints tc INNER JOIN information_schema.constraint_column_usage u ON tc.constraint_catalog = u.constraint_catalog AND tc.constraint_schema=u.constraint_schema AND tc.constraint_name=u.constraint_name INNER JOIN information_schema.columns c ON u.table_catalog=c.table_catalog AND u.table_schema=c.table_schema AND u.table_name=c.table_name AND u.column_name=c.column_name WHERE tc.constraint_type='PRIMARY KEY' AND tc.table_name=?) ORDER BY c.ordinal_position" [toPrimitivePersistValue proxy name, toPrimitivePersistValue proxy name] (mapAllRows $ return . getColumn name . fst . fromPurePersistValues proxy)
      let (col_errs, cols') = partitionEithers cols
      
      uniqRows <- queryRaw' "SELECT u.constraint_name, u.column_name FROM information_schema.table_constraints tc INNER JOIN information_schema.constraint_column_usage u ON tc.constraint_catalog=u.constraint_catalog AND tc.constraint_schema=u.constraint_schema AND tc.constraint_name=u.constraint_name WHERE u.table_name=? AND tc.constraint_type='UNIQUE' ORDER BY u.constraint_name, u.column_name" [toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
      let mkUniq us = UniqueDef' (fst $ head us) (map snd us)
      let uniqs' = map mkUniq $ groupBy ((==) `on` fst) uniqRows
      references <- checkTableReferences name
      primaryKeyResult <- checkPrimaryKey name
      let (primaryKey, uniqs'') = case primaryKeyResult of
            (Left primaryKeyName) -> (primaryKeyName, uniqs')
            (Right u) -> (Nothing, u:uniqs')
      return $ Just $ case col_errs of
        []   -> Right $ TableInfo primaryKey cols' uniqs'' references
        errs -> Left errs
    Nothing -> return Nothing

checkPrimaryKey :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m (Either (Maybe String) UniqueDef')
checkPrimaryKey name = do
  uniqRows <- queryRaw' "SELECT u.constraint_name, u.column_name FROM information_schema.table_constraints tc INNER JOIN information_schema.constraint_column_usage u ON tc.constraint_catalog = u.constraint_catalog AND tc.constraint_schema=u.constraint_schema AND tc.constraint_name=u.constraint_name WHERE tc.constraint_type='PRIMARY KEY' AND tc.table_name=?" [toPrimitivePersistValue proxy name] (mapAllRows $ return . fst . fromPurePersistValues proxy)
  let mkUniq us = UniqueDef' (fst $ head us) (map snd us)
  return $ case uniqRows of
    [] -> Left Nothing
    [(_, primaryKeyName)] -> Left $ Just primaryKeyName
    us -> Right $ mkUniq us

getColumn :: String -> (String, String, String, Maybe String) -> Either String (Column DbType)
getColumn _ (column_name, is_nullable, udt_name, d) = case readSqlType udt_name of
      Left s -> Left s
      Right t -> Right $ Column column_name (is_nullable == "YES") t d

checkTableReferences :: (MonadBaseControl IO m, MonadIO m) => String -> DbPersist Postgresql m [(Maybe String, Reference)]
checkTableReferences tableName = do
  let sql = "SELECT c.conname, c.foreign_table || '', a_child.attname AS child, a_parent.attname AS parent FROM (SELECT r.confrelid::regclass AS foreign_table, r.conrelid, r.confrelid, unnest(r.conkey) AS conkey, unnest(r.confkey) AS confkey, r.conname FROM pg_catalog.pg_constraint r WHERE r.conrelid = ?::regclass AND r.contype = 'f') AS c INNER JOIN pg_attribute a_parent ON a_parent.attnum = c.confkey AND a_parent.attrelid = c.confrelid INNER JOIN pg_attribute a_child ON a_child.attnum = c.conkey AND a_child.attrelid = c.conrelid ORDER BY c.conname"
  x <- queryRaw' sql [toPrimitivePersistValue proxy $ escape tableName] $ mapAllRows (return . fst . fromPurePersistValues proxy)
  -- (refName, (parentTable, (childColumn, parentColumn)))
  let mkReference xs = (Just refName, (parentTable, map (snd . snd) xs)) where
        (refName, (parentTable, _)) = head xs
      references = map mkReference $ groupBy ((==) `on` fst) x
  return references

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
showSqlType (DbList _ _) = showSqlType DbInt64
showSqlType (DbEntity Nothing _) = showSqlType DbInt64
showSqlType t = error $ "showSqlType: DbType does not have corresponding database type: " ++ show t

compareColumns :: Column DbType -> Column DbType -> Bool
compareColumns = ((==) `on` f) where
  f col = col {colType = simplifyType (colType col)}

compareUniqs :: UniqueDef' -> UniqueDef' -> Bool
compareUniqs (UniqueDef' name1 cols1) (UniqueDef' name2 cols2) = name1 == name2 && haveSameElems (==) cols1 cols2

compareRefs :: (Maybe String, Reference) -> (Maybe String, Reference) -> Bool
compareRefs (_, (tbl1, pairs1)) (_, (tbl2, pairs2)) = unescape tbl1 == unescape tbl2 && haveSameElems (==) pairs1 pairs2 where
  unescape name = if head name == '"' && last name == '"' then tail $ init name else name

-- | Converts complex datatypes that reference other data to id type DbInt64. Does not handle DbEmbedded
simplifyType :: DbType -> DbType
simplifyType (DbEntity Nothing _) = DbInt64
simplifyType (DbList _ _) = DbInt64
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
