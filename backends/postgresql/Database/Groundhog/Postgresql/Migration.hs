{-# LANGUAGE OverloadedStrings #-}
module Database.Groundhog.Postgresql.Migration (migrate') where

import Database.Groundhog.Core hiding (Update)
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Postgresql.Base

import Control.Arrow ((&&&))
import Control.Monad(liftM)
import Control.Monad.IO.Control (MonadControlIO)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Int(Int32)
import Data.List (intercalate, groupBy, sort)
import Data.Maybe (mapMaybe, maybeToList)

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
migrate' :: (PersistEntity v, MonadControlIO m) => v -> Migration (DbPersist Postgresql m)
migrate' = migrateRecursively migE migT migL where
  migE e = do
    let name = getEntityName e
    let constrs = constructors e
    let mainTableQuery = "CREATE TABLE " ++ escape name ++ " (id$ SERIAL PRIMARY KEY UNIQUE, discr$ INT4 NOT NULL)"
    let mainTableColumns = [Column "discr$" False DbInt32 Nothing Nothing]

    if isSimple constrs
      then do
        x <- checkTable name
        -- check whether the table was created for multiple constructors before
        case x of
          Just (Right (columns, _)) | columns == mainTableColumns -> do
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
              then mergeMigrations $ Right [(False, 0, mainTableQuery)]:map snd res
              else Left $ foldl (\l (_, c) -> ("Orphan constructor table found: " ++ constrTable c):l) [] $ filter (fst.fst) $ zip res constrs
          Just (Right (columns, [])) -> do
            if columns == mainTableColumns
              then do -- the datatype had also many constructors before
-- check whether any new constructors appeared and increment older discriminators, which were shifted by newer constructors inserted not in the end
                return $ if any (not.fst) res
                  then Left ["Migration with constructors addition will be implemented soon. Datatype: " ++ name]
                  else mergeMigrations $ map snd res
              else do
                return $ Left ["Migration from one constructor to many will be implemented soon. Datatype: " ++ name]
          Just (Right (_, constraints)) -> do
            return $ Left ["Unexpected constraints on main table datatype. Datatype: " ++ name ++ ". Constraints: " ++ show constraints]
          Just (Left errs) -> return (Left errs)
            
  -- we don't need any escaping because tuple table name and fields are always valid
  migT n ts = do
    let name = intercalate "$" $ ("Tuple" ++ show n ++ "$") : map getName ts
    let fields = zipWith (\i t -> ("val" ++ show i, t)) [0::Int ..] ts
    (_, trigger) <- migTriggerOnDelete name $ mkDeletesOnDelete fields
    let fields' = concatMap (\(s, t) -> sqlColumn s (getType t)) fields
    let query = "CREATE TABLE " ++ escape name ++ " (id$ SERIAL PRIMARY KEY UNIQUE" ++ fields' ++ ")"
    x <- checkTable name
    let expectedColumns = map (\(fname, ntype) -> mkColumn name fname ntype) fields
    let addReferences = mapMaybe (uncurry $ createReference name) fields
    return $ case x of
      Nothing  -> mergeMigrations $ Right [(False, 0, query)] : addReferences ++ [trigger]
      Just (Right (columns, [])) -> if columns == expectedColumns
        then Right []
        else Left ["Tuple table " ++ name ++ " has unexpected structure: " ++ show columns]
      Just (Right (_, constraints)) -> Left ["Tuple table " ++ name ++ " has unexpected constraints: " ++ show constraints]
      Just (Left errs) -> Left errs

  -- we should consider storing tuples as is, not their id. For example for [(a, b)] this will prevent many unnecessary queries
  migL t = do
    let mainName = "List$" ++ "$" ++ getName t
    let valuesName = mainName ++ "$" ++ "values"
    let mainQuery = "CREATE TABLE " ++ escape mainName ++ " (id$ SERIAL PRIMARY KEY UNIQUE)"
    let valuesQuery = "CREATE TABLE " ++ escape valuesName ++ " (id$ INTEGER, ord$ INTEGER NOT NULL" ++ sqlColumn "value" (getType t) ++ ")"
    x <- checkTable mainName
    y <- checkTable valuesName
    (_, triggerMain) <- migTriggerOnDelete mainName ["DELETE FROM " ++ valuesName ++ " WHERE id$=old.id$;"]
    (_, triggerValues) <- migTriggerOnDelete valuesName $ mkDeletesOnDelete [("value", t)]
    let f name a b = if a /= b then ["List table " ++ name ++ " error. Expected: " ++ show a ++ ". Found: " ++ show b] else []
    let expectedMainStructure = ([], [])
    let expectedValuesStructure = ([mkColumn valuesName "ord$" (namedType (0 :: Int32)), mkColumn valuesName "value" t], [])
    let addReferences = maybeToList $ createReference valuesName "value" t
    return $ case (x, y) of
      (Nothing, Nothing) -> mergeMigrations $ [Right [(False, 0, mainQuery), (False, 0, valuesQuery)]] ++ addReferences ++ [triggerMain, triggerValues]
      (Just (Right mainStructure), Just (Right valuesStructure)) -> let
        errors = f mainName expectedMainStructure mainStructure ++ f valuesName expectedValuesStructure valuesStructure
        in if null errors then Right [] else Left errors
      (Just (Left errs1), Just (Left errs2)) -> Left $ errs1 ++ errs2
      (Just (Left errs), Just _) -> Left errs
      (Just _, Just (Left errs)) -> Left errs
      (_, Nothing) -> Left ["Found orphan main list table " ++ mainName]
      (Nothing, _) -> Left ["Found orphan list values table " ++ valuesName]

createReference :: String -> String -> NamedType -> Maybe SingleMigration
createReference tname fname typ = fmap (\x -> Right $ [(False, 1, showAlter tname (fname, AddReference x))]) $ go typ where
  go x = case getType x of
    DbMaybe a   -> go a
    DbEntity t  -> Just $ getEntityName t
    DbTuple _ _ -> Just $ getName x
    DbList _    -> Just $ getName x
    _ -> Nothing

sqlColumn :: String -> DbType -> String
sqlColumn name typ = ", " ++ escape name ++ " " ++ showSqlType typ ++ f typ where
  f (DbMaybe t) = ""
  f t = " NOT NULL"
  
showColumn :: Column -> String
showColumn (Column n nu t def ref) = concat
    [ escape n
    , " "
    , showSqlType t
    , " "
    , if nu then "NULL" else "NOT NULL"
    , case def of
        Nothing -> ""
        Just s -> " DEFAULT " ++ s
    ]

sqlUnique :: Constraint -> String
sqlUnique (cname, cols) = concat
    [ ", CONSTRAINT "
    , escape cname
    , " UNIQUE ("
    , intercalate "," $ map escape cols
    , ")"
    ]

migConstrAndTrigger :: MonadControlIO m => Bool -> String -> ConstructorDef -> DbPersist Postgresql m (Bool, SingleMigration)
migConstrAndTrigger simple name constr = do
  let cName = if simple then name else name ++ [defDelim] ++ constrName constr
  (constrExisted, mig) <- migConstr (if simple then Nothing else Just name) cName constr
  let dels = mkDeletesOnDelete $ constrParams constr
  let allDels = if simple then dels else ("DELETE FROM " ++ escape name ++ " WHERE id=old." ++ constrId ++ ";"):dels
  (triggerExisted, delTrigger) <- migTriggerOnDelete cName allDels
  let updDels = mkDeletesOnUpdate $ constrParams constr
  updTriggers <- mapM (liftM snd . uncurry (migTriggerOnUpdate cName)) updDels
{-
  return $ if constrExisted == triggerExisted || (constrExisted && null allDels)
    then (constrExisted, mergeMigrations ([mig, delTrigger] ++ updTriggers))
    -- this can happen when an ephemeral field was added. Consider doing something else except throwing an error
    else (constrExisted, Left ["Trigger and constructor table must exist together: " ++ cName])
-}
  return (constrExisted, mergeMigrations ([mig, delTrigger] ++ updTriggers))

migConstr :: MonadControlIO m => Maybe String -> String -> ConstructorDef -> DbPersist Postgresql m (Bool, SingleMigration)
migConstr mainTableName cName constr = do
  let fields = constrParams constr
  let uniques = constrConstrs constr
  let new = mkColumns cName constr
  let mainRef = maybe "" (\x -> " REFERENCES " ++ escape x ++ " ON DELETE CASCADE ") mainTableName
  let addTable = "CREATE TABLE " ++ escape cName ++ " (" ++ constrId ++ " SERIAL PRIMARY KEY UNIQUE" ++ mainRef ++ concatMap (\(n, t) -> sqlColumn n (getType t)) fields ++ ")"
  let addReferences = mapMaybe (uncurry $ createReference cName) fields
  x <- checkTable cName
  return $ case x of
    Nothing  -> let
      rest = map (AlterTable cName . uncurry AddUniqueConstraint) uniques
      in (False, mergeMigrations $ (Right $ (map showAlterDb $ (AddTable addTable):rest)) : addReferences)
    Just (Right old) -> let
      (acs, ats) = getAlters new old
      acs' = map (AlterColumn cName) acs
      ats' = map (AlterTable cName) ats
      in (True, Right $ map showAlterDb $ acs' ++ ats')
    Just (Left errs) -> (True, Left errs)

-- it handles only delete operations. So far when list or tuple replace is not allowed, it is ok
migTriggerOnDelete :: MonadControlIO m => String -> [String] -> DbPersist Postgresql m (Bool, SingleMigration)
migTriggerOnDelete name deletes = return (False, Right [])
{-
migTriggerOnDelete name deletes = do
  let query = "CREATE TRIGGER " ++ escape name ++ " DELETE ON " ++ escape name ++ " BEGIN " ++ concat deletes ++ "END"
  x <- checkTrigger name
  return $ case x of
    Nothing | null deletes -> (False, Right [])
    Nothing -> (False, Right [(False, query)])
    Just sql -> (True, if null deletes -- remove old trigger if a datatype earlier had fields of ephemeral types
      then Right [(False, "DROP TRIGGER " ++ escape name)]
      else if sql == query
        then Right []
        -- this can happen when a field was added or removed. Consider trigger replacement.
        else Left ["The trigger " ++ name ++ " is different from expected. Manual migration required.\n" ++ sql ++ "\n" ++ query])
-}
      
-- | Table name and a  list of field names and according delete statements
-- assume that this function is called only for ephemeral fields
migTriggerOnUpdate :: MonadControlIO m => String -> String -> String -> DbPersist Postgresql m (Bool, SingleMigration)
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
-- TODO: merge several delete queries for a case when a constructor has several fields of the same ephemeral type
mkDeletesOnDelete :: [(String, NamedType)] -> [String]
mkDeletesOnDelete types = map (uncurry delField) ephemerals where
  -- we have the same query structure for tuples and lists
  delField field t = "DELETE FROM " ++ ephemeralTableName ++ " WHERE id=old." ++ escape field ++ ";" where
    ephemeralTableName = go t
    go a = case getType a of
      DbMaybe x -> go x
      _         -> getName a
  ephemerals = filter (isEphemeral.snd) types
  
-- on delete removes all ephemeral data
mkDeletesOnUpdate :: [(String, NamedType)] -> [(String, String)]
mkDeletesOnUpdate types = map (uncurry delField) ephemerals where
  -- we have the same query structure for tuples and lists
  delField field t = (field, "DELETE FROM " ++ ephemeralTableName ++ " WHERE id=old." ++ escape field ++ ";") where
    ephemeralTableName = go t
    go a = case getType a of
      DbMaybe x -> go x
      _         -> getName a
  ephemerals = filter (isEphemeral.snd) types

isEphemeral :: NamedType -> Bool
isEphemeral a = case getType a of
  DbMaybe x   -> isEphemeral x
  DbList _    -> True
  DbTuple _ _ -> True
  _           -> False
  
data Column = Column
    { cName :: String
    , cNull :: Bool
    , cType :: DbType
    , cDefault :: Maybe String
    , cReference :: Maybe (String, String) -- table name, constraint name
    } deriving (Eq, Show)

checkTable :: MonadControlIO m => String -> DbPersist Postgresql m (Maybe (Either [String] ([Column], [Constraint])))
checkTable name = do
  table <- queryRaw' "SELECT * FROM information_schema.tables WHERE table_name=?" [toPrim name] firstRow
  case table of
    Just _ -> do
      cols <- queryRaw' "SELECT column_name,is_nullable,udt_name,column_default FROM information_schema.columns WHERE table_name=? AND column_name <> 'id$' ORDER BY ordinal_position" [toPrim name] (mapAllRows $ getColumn name)
      let (col_errs, cols') = partitionEithers cols
      
      let helperU [con, col] = Right (fromPrim con, fromPrim col)
          helperU x = Left $ "Invalid result from information_schema.constraint_column_usage: " ++ show x
      rawUniqs <- queryRaw' "SELECT constraint_name, column_name FROM information_schema.constraint_column_usage WHERE table_name=? AND column_name <> 'id$' ORDER BY constraint_name, column_name" [toPrim name] (mapAllRows $ return . helperU)
      let (uniq_errs, uniqRows) = partitionEithers rawUniqs
      let uniqs' = map (fst . head &&& map snd) $ groupBy ((==) `on` fst) uniqRows

      return $ Just $ case col_errs ++ uniq_errs of
        []   -> Right (cols', uniqs')
        errs -> Left errs
    Nothing -> return Nothing

getColumn :: MonadControlIO m => String -> [PersistValue] -> DbPersist Postgresql m (Either String Column)
getColumn tname [column_name, PersistByteString is_nullable, udt_name, d] =
    case d' of
        Left s -> return $ Left s
        Right d'' ->
            case readSqlType $ fromPrim udt_name of
                Left s -> return $ Left s
                Right t -> do
                    let cname = fromPrim column_name
                    ref <- getRef cname
                    return $ Right $ Column cname (is_nullable == "YES") t d'' ref
  where
    getRef cname = do
        let sql = "SELECT u.table_name FROM information_schema.table_constraints tc INNER JOIN information_schema.constraint_column_usage u ON tc.constraint_catalog=u.constraint_catalog AND tc.constraint_schema=u.constraint_schema AND tc.constraint_name=u.constraint_name WHERE tc.table_name=? AND tc.constraint_type='FOREIGN KEY' AND tc.constraint_name=?"
        let ref = refName tname cname
        x <- queryRaw' sql [toPrim tname, toPrim ref] firstRow
        return $ fmap (\name -> (fromPrim $ head name, ref)) x
    d' = case d of
            PersistNull -> Right Nothing
            a@(PersistByteString _) -> Right $ Just $ fromPrim a
            _ -> Left $ "Invalid default column: " ++ show d
getColumn _ x = return $ Left $ "Invalid result from information_schema: " ++ show x

data AlterColumn = Type DbType | IsNull | NotNull | Add Column | Drop
                 | Default String | NoDefault | Update String
                 | AddReference String | DropReference String
type AlterColumn' = (String, AlterColumn)

data AlterTable = AddUniqueConstraint String [String]
                | DropConstraint String

data AlterDB = AddTable String
             | AlterColumn String AlterColumn'
             | AlterTable String AlterTable

getAlters :: ([Column], [Constraint])
          -> ([Column], [Constraint])
          -> ([AlterColumn'], [AlterTable])
getAlters (c1, u1) (c2, u2) =
    (getAltersC c1 c2, getAltersU u1 u2)
  where
    getAltersC [] old = map (\x -> (cName x, Drop)) old
    getAltersC (new:news) old =
        let (alters, old') = findAlters new old
         in alters ++ getAltersC news old'
    getAltersU [] old = map (DropConstraint . fst) old
    getAltersU ((name, cols):news) old =
        case lookup name old of
            Nothing -> AddUniqueConstraint name cols : getAltersU news old
            Just ocols ->
                let old' = filter (\(x, _) -> x /= name) old
                 in if sort cols == ocols
                        then getAltersU news old'
                        else  DropConstraint name
                            : AddUniqueConstraint name cols
                            : getAltersU news old'

findAlters :: Column -> [Column] -> ([AlterColumn'], [Column])
findAlters col@(Column name isNull type_ def ref) cols =
    case filter (\c -> cName c == name) cols of
        [] -> ((name, Add col) : (maybeToList $ fmap (\x -> (name, AddReference $ fst x)) ref), cols)
        Column _ isNull' type_' def' ref':_ ->
            let refDrop Nothing = []
                refDrop (Just (_, cname)) = [(name, DropReference cname)]
                refAdd Nothing = []
                refAdd (Just (tname, _)) = [(name, AddReference tname)]
                modRef =
                    if fmap snd ref == fmap snd ref'
                        then []
                        else refDrop ref' ++ refAdd ref
                modNull = case (isNull, isNull') of
                            (True, False) -> [(name, IsNull)]
                            (False, True) ->
                                let up = case def of
                                            Nothing -> id
                                            Just s -> (:) (name, Update s)
                                 in up [(name, NotNull)]
                            _ -> []
                modType = if type_ == type_' then [] else [(name, Type type_)]
                modDef =
                    if def == def'
                        then []
                        else case def of
                                Nothing -> [(name, NoDefault)]
                                Just s -> [(name, Default s)]
             in (modRef ++ modDef ++ modNull ++ modType,
                 filter (\c -> cName c /= name) cols)

showAlterDb :: AlterDB -> (Bool, Int, String)
showAlterDb (AddTable s) = (False, 0, s)
showAlterDb (AlterColumn t (c, ac)) =
    (isUnsafe ac, order, showAlter t (c, ac))
  where
    isUnsafe Drop = True
    isUnsafe _ = False
    order = case ac of
      AddReference _ -> 1
      _              -> 0
showAlterDb (AlterTable t at) = (False, 0, showAlterTable t at)

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
showAlter table (n, AddReference t2) = concat
    [ "ALTER TABLE "
    , escape table
    , " ADD CONSTRAINT "
    , escape $ refName table n
    , " FOREIGN KEY("
    , escape n
    , ") REFERENCES "
    , escape t2
    ]
showAlter table (_, DropReference cname) =
    "ALTER TABLE " ++ escape table ++ " DROP CONSTRAINT " ++ escape cname
    
-- TODO: move all code below to generic modules

refName :: String -> String -> String
refName table column = table ++ '_' : column ++ "_fkey"

-- | Create the list of columns for the given entity.
mkColumns :: String -- ^ constructor table name
          -> ConstructorDef -- constructor definition
          -> ([Column], [Constraint])
mkColumns cname constr = (cols, uniqs) where
    uniqs = constrConstrs constr
    cols = map (\(fname, ntype) -> mkColumn cname fname ntype) $ constrParams constr

mkColumn :: String -> String -> NamedType -> Column
mkColumn tableName columnName dbtype = Column columnName isNullable (simpleType dbtype) Nothing ref where
  reference = refName tableName columnName
  simpleType x = case getType x of
    DbMaybe a   -> simpleType a
    DbEntity _  -> DbInt32
    DbTuple _ _ -> DbInt32
    DbList _    -> DbInt32
    a -> a
  (isNullable, ref) = f $ getType dbtype where 
    f (DbMaybe t) = (True, g t)
    f t = (False, g dbtype)
    g x = case getType x of
      DbEntity _  -> Just (getName x, reference)
      DbTuple _ _ -> Just (getName x, reference)
      DbList _    -> Just (getName x, reference)
      _           -> Nothing

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
showSqlType (DbMaybe t) = showSqlType (getType t)
showSqlType (DbList _) = "INTEGER"
showSqlType (DbTuple _ _) = "INTEGER"
showSqlType (DbEntity _) = "INTEGER"
