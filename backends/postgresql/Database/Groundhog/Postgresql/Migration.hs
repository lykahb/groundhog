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
import Data.List (intercalate, groupBy, sort)

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
    let mainTableQuery = "CREATE TABLE " ++ escape name ++ " (id INTEGER PRIMARY KEY, discr INT4 NOT NULL)"
    let mainTableColumns = [Column "discr" False DbInt32 Nothing Nothing]

    if isSimple constrs
      then do
        x <- checkTable2 name
        -- check whether the table was created for multiple constructors before
        case x of
          Just (Right (columns, [])) | columns == mainTableColumns -> do
            return $ Left ["Datatype with multiple constructors was truncated to one constructor. Manual migration required. Datatype: " ++ name]
          Just (Right (_, constraints)) -> do
            return $ Left ["Unexpected constraints on main table datatype. Datatype: " ++ name ++ ". Constraints: " ++ show constraints]
          Just (Left errs) -> return (Left errs)
          _ -> liftM snd $ migConstrAndTrigger True name $ head constrs
      else do
        maincolumns <- checkTable2 name
        let constrTable c = name ++ [defDelim] ++ constrName c
        res <- mapM (\c -> migConstrAndTrigger False name c) constrs
        case maincolumns of
          Nothing -> do
            -- no constructor tables can exist if there is no main data table
            let orphans = filter fst res
            return $ if null orphans
              then mergeMigrations $ Right [(False, mainTableQuery)]:map snd res
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
    let query = "CREATE TABLE " ++ name ++ " (id INTEGER PRIMARY KEY" ++ fields' ++ ")"
    x <- checkTable2 name
    let expectedColumns = map (\(fname, ntype) -> mkColumn name fname (getType ntype)) fields
    return $ case x of
      Nothing  -> mergeMigrations [Right [(False, query)], trigger]
      Just (Right (columns, [])) -> if columns == expectedColumns
        then Right []
        else Left ["Tuple table " ++ name ++ " has unexpected structure: " ++ show columns]
      Just (Right (_, constraints)) -> Left ["Tuple table " ++ name ++ " has unexpected constraints: " ++ show constraints]
      Just (Left errs) -> Left errs

  -- we should consider storing tuples as is, not their id. For example for [(a, b)] this will prevent many unnecessary queries
  --TODO:finish
  migL t = do
    let mainName = "List$" ++ "$" ++ getName t
    let valuesName = mainName ++ "$" ++ "values"
    let mainQuery = "CREATE TABLE " ++ mainName ++ " (id INTEGER PRIMARY KEY)"
    let valuesQuery = "CREATE TABLE " ++ valuesName ++ " (id INTEGER, ord$ INTEGER NOT NULL" ++ sqlColumn "value" (getType t) ++ ")"
    x <- checkTable2 mainName
    y <- checkTable2 valuesName
    (_, triggerMain) <- migTriggerOnDelete mainName ["DELETE FROM " ++ valuesName ++ " WHERE id=old.id;"]
    (_, triggerValues) <- migTriggerOnDelete valuesName $ mkDeletesOnDelete [("value", t)]
    let f name a b = if a /= b then ["List table " ++ name ++ " error. Expected: " ++ show a ++ ". Found: " ++ show b] else []
    let expectedMainStructure = ([], [])
    let expectedValuesStructure = ([mkColumn valuesName "ord$" DbInt32, mkColumn valuesName "value" (getType t)], [])
    return $ case (x, y) of
      (Nothing, Nothing) -> mergeMigrations [Right [(False, mainQuery), (False, valuesQuery)], triggerMain, triggerValues]
      (Just (Right mainStructure), Just (Right valuesStructure)) -> let
        errors = f mainName expectedMainStructure mainStructure ++ f valuesName expectedValuesStructure valuesStructure
        in if null errors then Right [] else Left errors
      (Just (Left errs1), Just (Left errs2)) -> Left $ errs1 ++ errs2
      (Just (Left errs), Just _) -> Left errs
      (Just _, Just (Left errs)) -> Left errs
      (_, Nothing) -> Left ["Found orphan main list table " ++ mainName]
      (Nothing, _) -> Left ["Found orphan list values table " ++ valuesName]

sqlColumn :: String -> DbType -> String
sqlColumn name typ = ", " ++ escape name ++ " " ++ showSqlType typ ++ f typ where
  f (DbMaybe t) = g (getType t)
  f t = " NOT NULL" ++ g t
  -- TODO: add references for tuple and list
  g (DbEntity t) = " REFERENCES " ++ escape (getEntityName t)
  g (DbTuple n ts) = " REFERENCES " ++ (intercalate "$" $ ("Tuple" ++ show n ++ "$") : map getName ts)
  g (DbList t) = " REFERENCES " ++ "List$$" ++ getName t
  g _ = ""
  
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
    , case ref of
        Nothing -> ""
        Just (s, _) -> " REFERENCES " ++ escape s
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
  (constrExisted, mig) <- migConstr cName constr
  let dels = mkDeletesOnDelete $ constrParams constr
  let allDels = if simple then dels else ("DELETE FROM " ++ escape name ++ " WHERE id=old." ++ constrId ++ ";"):dels
  (triggerExisted, delTrigger) <- migTriggerOnDelete cName allDels
  let updDels = mkDeletesOnUpdate $ constrParams constr
  updTriggers <- mapM (liftM snd . uncurry (migTriggerOnUpdate cName)) updDels
  return $ if constrExisted == triggerExisted || (constrExisted && null allDels)
    then (constrExisted, mergeMigrations ([mig, delTrigger] ++ updTriggers))
    -- this can happen when an ephemeral field was added. Consider doing something else except throwing an error
    else (constrExisted, Left ["Trigger and constructor table must exist together: " ++ cName])

migConstr :: MonadControlIO m => String -> ConstructorDef -> DbPersist Postgresql m (Bool, SingleMigration)
migConstr name constr = do
  let fields = constrParams constr
  let uniques = constrConstrs constr
  let new = mkColumns name constr
  let addTable = "CREATE TABLE " ++ escape name ++ " (" ++ constrId ++ " SERIAL PRIMARY KEY UNIQUE" ++ concatMap (\(n, t) -> sqlColumn n (getType t)) fields ++ ")"
  x <- checkTable2 name
  return $ case x of
    Nothing  -> let
      rest = map (AlterTable name . uncurry AddUniqueConstraint) uniques
      in (False, Right $ map showAlterDb $ (AddTable addTable):rest)
    Just (Right old) -> let
      (acs, ats) = getAlters new old
      acs' = map (AlterColumn name) acs
      ats' = map (AlterTable name) ats
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

checkTable2 :: MonadControlIO m => String -> DbPersist Postgresql m (Maybe (Either [String] ([Column], [Constraint])))
checkTable2 name = do
  table <- queryRaw' "SELECT * FROM information_schema.tables WHERE table_name=?" [toPrim name] firstRow
  case table of
    Just _ -> do
      cols <- queryRaw' "SELECT column_name,is_nullable,udt_name,column_default FROM information_schema.columns WHERE table_name=? AND column_name <> 'id' ORDER BY ordinal_position" [toPrim name] (mapAllRows $ getColumn name)
      let (col_errs, cols') = partitionEithers cols
      
      let helperU [con, col] = Right (fromPrim con, fromPrim col)
          helperU x = Left $ "Invalid result from information_schema.constraint_column_usage: " ++ show x
      rawUniqs <- queryRaw' "SELECT constraint_name, column_name FROM information_schema.constraint_column_usage WHERE table_name=? AND column_name <> 'id' ORDER BY constraint_name, column_name" [toPrim name] (mapAllRows $ return . helperU)
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
        let sql = "SELECT COUNT(*) FROM information_schema.table_constraints WHERE table_name=? AND constraint_type='FOREIGN KEY' AND constraint_name=?"
        let ref = refName tname cname
        Just [PersistInt64 i] <- queryRaw' sql [toPrim tname, toPrim ref] firstRow
        return $ if i == 0 then Nothing else Just ("", ref)
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
        [] -> ([(name, Add col)], cols)
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

showAlterDb :: AlterDB -> (Bool, String)
showAlterDb (AddTable s) = (False, s)
showAlterDb (AlterColumn t (c, ac)) =
    (isUnsafe ac, showAlter t (c, ac))
  where
    isUnsafe Drop = True
    isUnsafe _ = False
showAlterDb (AlterTable t at) = (False, showAlterTable t at)

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
    cols = map (\(fname, ntype) -> mkColumn cname fname (getType ntype)) $ constrParams constr

--    go :: (String, String, attrs) -> [(String, NamedType)] ->
--    def [] = Nothing
--    def (('d':'e':'f':'a':'u':'l':'t':'=':d):_) = Just d
--    def (_:rest) = def rest
    
mkColumn :: String -> String -> DbType -> Column
mkColumn tableName columnName dbtype = Column columnName isNullable dbtype Nothing ref where
  reference = refName tableName columnName
  {- isNullable = case dbtype of
    DbMaybe _ -> True
    _ -> False
    
  ref :: Maybe (String, String)
  ref = case dbtype of
    (DbEntity t)   -> Just (getEntityName t, reference)
    (DbTuple n ts) -> Just (intercalate "$" $ ("Tuple" ++ show n ++ "$") : map getName ts, reference)
    (DbList t)     -> Just ("List$$" ++ getName t, reference)
    _ -> Nothing
    -}
  --() :: (Bool, Maybe (String, String))
  (isNullable, ref) = f dbtype where 
    f (DbMaybe t) = (True, g (getType t))
    f t = (False, g t)
    g (DbEntity t)   = Just (getEntityName t, reference)
    g (DbTuple n ts) = Just (intercalate "$" $ ("Tuple" ++ show n ++ "$") : map getName ts, reference)
    g (DbList t)     = Just ("List$$" ++ getName t, reference)
    g _ = Nothing
  
{-  
-- | Create the list of columns for the given entity.
mkColumns :: String -- ^ constructor table name
          -> ConstructorDef -- constructor definition
          -> ([Column], [UniqueDef'])
mkColumns cname constr = (cols, uniqs) where
    uniqs = constrConstrs constr
    cols = map go $ constrParams constr

--    go :: (String, String, attrs) -> [(String, NamedType)] ->
    go :: (String, NamedType) -> Column
    go (name, ntype) = Column name (nullable dbtype) (dbtype) (def []) (ref name dbtype) where
      dbtype = getType ntype

    def [] = Nothing
    def (('d':'e':'f':'a':'u':'l':'t':'=':d):_) = Just d
    def (_:rest) = def rest
    
    nullable (DbMaybe _) = True
    nullable _ = False
    
    ref :: String -> DbType -> Maybe (String, String)
    ref name (DbEntity t) = Just (getEntityName t, refName cname name)
    ref name (DbTuple n ts) = Just (intercalate "$" $ ("Tuple" ++ show n ++ "$") : map getName ts, refName cname name)
    ref name (DbList t) = Just ("List$$" ++ getName t, refName cname name)
    ref _ _ = Nothing
-}
{-    
Person
    name String
    age Int Maybe
BlogPost
    title String sql=title
    authorId PersonId
    -}
--    if type is PersonId then reference table Person and constraint name BlogPost_authorId_fkey
    -- column name -> type name -> attributes
{-    ref :: String -> String -> String -> Maybe (String, String)
    ref c t' [] =
        let l = length t'
            (f, b) = splitAt (l - 2) t'
         in if b == "Id"
                then Just (RawName f, refName tn c)
                else Nothing
    ref _ _ ("noreference":_) = Nothing
    ref c _ (('r':'e':'f':'e':'r':'e':'n':'c':'e':'=':x):_) =
        Just (RawName x, refName tn c)
    ref c x (_:y) = ref c x y -}
    
--    name, typename, [atributes]
--tableColumns :: EntityDef -> [(RawName, String, [String])]

{-sqlColumn :: String -> DbType -> String
sqlColumn name typ = ", " ++ escape name ++ " " ++ showSqlType typ ++ f typ where
  f (DbMaybe t) = g (getType t)
  f t = " NOT NULL" ++ g t
  -- TODO: add references for tuple and list
  g (DbEntity t) = " REFERENCES " ++ escape (getEntityName t)
  g (DbTuple n ts) = " REFERENCES " ++ (intercalate "$" $ ("Tuple" ++ show n ++ "$") : map getName ts)
  g (DbList t) = " REFERENCES " ++ "List$$" ++ getName t
  g _ = ""
-}

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
