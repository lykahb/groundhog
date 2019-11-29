{-# LANGUAGE FlexibleContexts, OverloadedStrings, FlexibleInstances, TypeFamilies, RecordWildCards, ScopedTypeVariables#-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module defines the functions which are used only for backends creation.
module Database.Groundhog.Generic.Sql
    (
    -- * SQL rendering utilities
      renderCond
    , defaultShowPrim
    , renderOrders
    , renderUpdates
    , renderFields
    , renderChain
    , renderExpr
    , renderExprPriority
    , renderExprExtended
    , renderPersistValue
    , mkExprWithConf
    , prerenderExpr
    , intercalateS
    , commasJoin
    , flatten
    , RenderS(..)
    , Utf8(..)
    , fromUtf8
    , RenderConfig(..)
    , StringLike(..)
    , fromString
    , function
    , operator
    , parens
    , mkExpr
    , Snippet(..)
    , SqlDb(..)
    , FloatingSqlDb(..)
    , tableName
    , mainTableName
    ) where

import Database.Groundhog.Core
import Database.Groundhog.Generic (isSimple)
import Database.Groundhog.Instances ()
import qualified Data.Text.Lazy.Builder as B
import Data.Maybe (mapMaybe, maybeToList)
import Data.String
import Data.Semigroup (Semigroup)

import Database.Groundhog.Expression


class (Semigroup a, Monoid a, IsString a) => StringLike a where
  fromChar :: Char -> a

data RenderS db r = RenderS {
    getQuery  :: Utf8
  , getValues :: [PersistValue] -> [PersistValue]
}

instance StringLike Utf8 where
  fromChar = Utf8 . B.singleton

-- | Escape function, priority of the outer operator. The result is a list for the embedded data which may expand to several RenderS.
newtype Snippet db r = Snippet (RenderConfig -> Int -> [RenderS db r])

newtype RenderConfig = RenderConfig {
    esc  :: Utf8 -> Utf8
}

-- | This class distinguishes databases which support SQL-specific expressions. It contains ad hoc members for features whose syntax differs across the databases.
class (DbDescriptor db, QueryRaw db ~ Snippet db) => SqlDb db where
  append :: (ExpressionOf db r a String, ExpressionOf db r b String) => a -> b -> Expr db r String
  signum' :: (ExpressionOf db r x a, Num a) => x -> Expr db r a
  quotRem' :: (ExpressionOf db r x a, ExpressionOf db r y a, Integral a) => x -> y -> (Expr db r a, Expr db r a)

  equalsOperator :: RenderS db r -> RenderS db r -> RenderS db r
  notEqualsOperator :: RenderS db r -> RenderS db r -> RenderS db r

-- | This class distinguishes databases which support trigonometry and other math functions. For example, PostgreSQL has them but Sqlite does not. It contains ad hoc members for features whose syntax differs across the databases.
class SqlDb db => FloatingSqlDb db where
  -- | Natural logarithm
  log' :: (ExpressionOf db r x a, Floating a) => x -> Expr db r a
  logBase' :: (ExpressionOf db r b a, ExpressionOf db r x a, Floating a) => b -> x -> Expr db r a

-- | If we reuse complex expression several times, prerendering it saves time. `RenderConfig` can be obtained with `mkExprWithConf`
prerenderExpr :: forall db r a. (SqlDb db, PersistField a) => RenderConfig -> Expr db r a -> Expr db r a
prerenderExpr conf (Expr e) = Expr $ ExprRaw typ $ Snippet $ \_ _ -> prerendered where
  proxy = undefined :: proxy db
  typ = dbType proxy (undefined :: a)
  -- Priority of outer operation is not known. Assuming that it is high ensures that parentheses won't be missing.
  prerendered = renderExprExtended conf maxBound e

-- | Helps creating an expression which depends on render configuration. It can be used in pair with `prerenderExpr`.
-- @
-- myExpr x = mkExprWithConf $ \conf _ -> let
--        x' = prerenderExpr conf x
--     in x' + x' * x'@
-- @
mkExprWithConf :: (SqlDb db, PersistField a) => (RenderConfig -> Int -> Expr db r a) -> Expr db r a
mkExprWithConf f = expr where
  expr = mkExpr $ Snippet $ \conf p -> [renderExprPriority conf p $ toExpr $ (f conf p) `asTypeOf` expr]

renderExpr :: SqlDb db => RenderConfig -> UntypedExpr db r -> RenderS db r
renderExpr conf expr = renderExprPriority conf 0 expr

renderExprPriority :: SqlDb db => RenderConfig -> Int -> UntypedExpr db r -> RenderS db r
renderExprPriority conf p expr = (case expr of
  ExprRaw _ (Snippet f) -> explicitHead (f conf p)
  ExprField f -> let fs = renderChain conf f []
                 in RenderS (explicitHead fs) id
  ExprPure  a -> let vals = toPurePersistValues a
                 in renderPersistValue $ explicitHead (vals [])
  ExprCond  a -> case renderCondPriority conf p a of
                   Nothing -> error "renderExprPriority: empty condition"
                   Just x -> x) where
    explicitHead :: [a] -> a
    explicitHead xs = case xs of
      [x] -> x
      xs' -> error $ "renderExprPriority: expected one column field, found " ++ show (length xs')

renderExprExtended :: SqlDb db => RenderConfig -> Int -> UntypedExpr db r -> [RenderS db r]
renderExprExtended conf p expr = (case expr of
  ExprRaw _ (Snippet f) -> f conf p
  ExprField f -> map (flip RenderS id) $ renderChain conf f []
  ExprPure a -> let vals = toPurePersistValues a []
                in map renderPersistValue vals
  ExprCond a -> maybeToList $ renderCondPriority conf p a) where

renderPersistValue :: PersistValue -> RenderS db r
renderPersistValue (PersistCustom s as) = RenderS s (as++)
renderPersistValue a = RenderS (fromChar '?') (a:)

instance Semigroup (RenderS db r) where
  (RenderS f1 g1) <> (RenderS f2 g2) = RenderS (f1 <> f2) (g1 . g2)

instance Monoid (RenderS db r) where
  mempty = RenderS mempty id
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance IsString (RenderS db r) where
  fromString s = RenderS (fromString s) id

instance StringLike (RenderS db r) where
  fromChar c = RenderS (fromChar c) id

-- Has bad performance. This instance exists only for testing purposes
instance StringLike String where
  fromChar c = [c]

{-# INLINABLE parens #-}
parens :: Int -> Int -> RenderS db r -> RenderS db r
parens p1 p2 expr = if p1 < p2 then fromChar '(' <> expr <> fromChar ')' else expr

operator :: (SqlDb db, Expression db r a, Expression db r b) => Int -> String -> a -> b -> Snippet db r
operator pr op = \a b -> Snippet $ \conf p ->
  [parens pr p $ renderExprPriority conf pr (toExpr a) <> fromString op <> renderExprPriority conf pr (toExpr b)]

function :: SqlDb db => String -> [UntypedExpr db r] -> Snippet db r
function func args = Snippet $ \conf _ -> [fromString func <> fromChar '(' <> commasJoin (map (renderExpr conf) args) <> fromChar ')']

mkExpr :: forall db r a . (SqlDb db, PersistField a) => Snippet db r -> Expr db r a
mkExpr snippet = Expr $ ExprRaw typ snippet where
  proxy = undefined :: proxy db
  typ = dbType proxy (undefined :: a)


{-# INLINABLE renderCond #-}
-- | Renders conditions for SQL backend. Returns Nothing if the fields don't have any columns.
renderCond :: SqlDb db
  => RenderConfig
  -> Cond db r -> Maybe (RenderS db r)
renderCond conf cond = renderCondPriority conf 0 cond

flattenNullables :: DbType -> [Bool]
flattenNullables typ = go typ [] where
  go :: DbType -> [Bool] -> [Bool]
  go typ acc = case typ of
    DbEmbedded (EmbeddedDef _ ts) _ -> foldr go acc $ map snd ts
    DbList _ _ -> False : acc
    DbTypePrimitive _ nullable _ _ -> nullable : acc

{-# INLINABLE renderCondPriority #-}
-- | Renders conditions for SQL backend. Returns Nothing if the fields don't have any columns.
renderCondPriority :: forall db r . SqlDb db
  => RenderConfig
  -> Int -> Cond db r -> Maybe (RenderS db r)
renderCondPriority conf@RenderConfig{..} priority cond = go cond priority where
  go (And a b)       p = perhaps andP p " AND " a b
  go (Or a b)        p = perhaps orP p " OR " a b
  go (Not CondEmpty) _ = Just "(1=0)" -- special case for False
  go (Not a)         p = fmap (\a' -> parens notP p $ "NOT " <> a') $ go a notP
  go (Compare compOp f1 f2) p = (case compOp of
    Eq -> renderCompOps andP " AND " 37 ops f1 f2 where
      ops = map (\isNull -> if isNull then equalsOperator else (\a b -> a <> fromChar '=' <> b)) eitherNullable
    Ne -> renderCompOps andP " OR " 50 ops f1 f2 where
      ops = map (\isNull -> if isNull then notEqualsOperator else (\a b -> a <> "<>" <> b)) eitherNullable
    Gt -> renderComp orP " OR " 38 (\a b -> a <> fromChar '>' <> b) f1 f2
    Lt -> renderComp orP " OR " 38 (\a b -> a <> fromChar '<' <> b) f1 f2
    Ge -> renderComp orP " OR " 38 (\a b -> a <> ">=" <> b) f1 f2
    Le -> renderComp orP " OR " 38 (\a b -> a <> "<=" <> b) f1 f2) where
      proxy = undefined :: proxy db

      eitherNullable = zipWith (||) (nullables f1) (nullables f2)

      nullables expr = case expr of
        ExprRaw t _ -> flattenNullables t
        ExprField ((_, t), _) -> flattenNullables t
        ExprPure a -> flattenNullables $ dbType proxy a
        ExprCond a -> [False]

      renderZip opP op expr1 expr2 = zipWith op expr1' expr2' where
        expr1' = renderExprExtended conf opP expr1
        expr2' = renderExprExtended conf opP expr2

      renderZip3 opP ops expr1 expr2 = zipWith3 (\op e1 e2 -> op e1 e2) ops expr1' expr2' where
        expr1' = renderExprExtended conf opP expr1
        expr2' = renderExprExtended conf opP expr2

      groupComparisons interP interOp opP comparisons = case comparisons of
        [] -> Nothing
        [clause] -> Just $ parens (opP - 1) p clause  -- put lower priority to make parentheses appear when the same operator is nested
        clauses  -> Just $ parens interP p $ intercalateS interOp clauses

      renderComp interP interOp opP op expr1 expr2 = groupComparisons interP interOp opP $ renderZip opP op expr1 expr2
      renderCompOps interP interOp opP ops expr1 expr2 = groupComparisons interP interOp opP $ renderZip3 opP ops expr1 expr2

  go (CondRaw (Snippet f)) p = case f conf p of
    [] -> Nothing
    [a] -> Just a
    _ -> error "renderCond: cannot render CondRaw with many elements"
  go CondEmpty _ = Nothing

  notP = 35
  andP = 30
  orP = 20

  perhaps p pOuter op a b = result where
    -- we don't know if the current operator is present until we render both operands. Rendering requires priority of the outer operator. We tie a knot to defer calculating the priority
    (p', result) = case (go a p', go b p') of
       (Just a', Just b') -> (p, Just $ parens p pOuter $ a' <> RenderS op id <> b')
       (Just a', Nothing) -> (pOuter, Just a')
       (Nothing, Just b') -> (pOuter, Just b')
       (Nothing, Nothing) -> (pOuter, Nothing)

{-
examples of prefixes
[("val1", EmbeddedDef False _), ("val4", EmbeddedDef False _), ("val5", EmbeddedDef False _)] -> "val5$val4$val1"
[("val1", EmbeddedDef True _),  ("val4", EmbeddedDef False _), ("val5", EmbeddedDef False _)] -> ""
[("val1", EmbeddedDef False _), ("val4", EmbeddedDef True _),  ("val5", EmbeddedDef False _)] -> "val1"
[("val1", EmbeddedDef False _), ("val4", EmbeddedDef True _),  ("val5", EmbeddedDef True _)] -> "val1"
[("val1", EmbeddedDef False _), ("val4", EmbeddedDef False _), ("val5", EmbeddedDef True _)] -> "val4$val1"
-}
{-# INLINABLE renderChain #-}
renderChain :: RenderConfig -> FieldChain -> [Utf8] -> [Utf8]
renderChain RenderConfig{..} (f, prefix) acc = (case prefix of
  ((name, EmbeddedDef False _):fs) -> flattenP esc (Just $ goP (fromString name) fs) f acc
  _ -> flatten esc f acc) where
  goP p ((name, EmbeddedDef False _):fs) = goP (fromString name <> fromChar delim <> p) fs
  goP p _ = p

defaultShowPrim :: PersistValue -> String
defaultShowPrim (PersistString x) = "'" ++ x ++ "'"
defaultShowPrim (PersistText x) = "'" ++ show x ++ "'"
defaultShowPrim (PersistByteString x) = "'" ++ show x ++ "'"
defaultShowPrim (PersistInt64 x) = show x
defaultShowPrim (PersistDouble x) = show x
defaultShowPrim (PersistBool x) = if x then "1" else "0"
defaultShowPrim (PersistDay x) = show x
defaultShowPrim (PersistTimeOfDay x) = show x
defaultShowPrim (PersistUTCTime x) = show x
defaultShowPrim (PersistZonedTime x) = show x
defaultShowPrim (PersistNull) = "NULL"
defaultShowPrim (PersistCustom _ _) = error "Unexpected PersistCustom"

{-# INLINABLE renderOrders #-}
renderOrders :: SqlDb db => RenderConfig -> [Order db r] -> RenderS db r
renderOrders _ [] = mempty
renderOrders conf xs = if null orders then mempty else " ORDER BY " <> commasJoin orders where
  orders = concatMap go xs
  rend conf' a = map (commasJoin . renderExprExtended conf' 0) $ projectionExprs a []
  go (Asc a) = rend conf a
  go (Desc a) = rend conf' a where
     conf' = conf { esc = \f -> esc conf f <> " DESC" }

{-# INLINABLE renderFields #-}
-- Returns string with comma separated escaped fields like "name,age"
-- If there are other columns before renderFields result, do not put comma because the result might be an empty string. This happens when the fields have no columns like ().
-- One of the solutions is to add one more field with datatype that is known to have columns, eg renderFields id (("id", namedType (0 :: Int64)) : constrParams constr)
{-# SPECIALIZE renderFields :: (Utf8 -> Utf8) -> [(String, DbType)] -> Utf8 #-}
renderFields :: StringLike s => (s -> s) -> [(String, DbType)] -> s
renderFields escape = commasJoin . foldr (flatten escape) []

{-# SPECIALIZE flatten :: (Utf8 -> Utf8) -> (String, DbType) -> ([Utf8] -> [Utf8]) #-}
flatten :: StringLike s => (s -> s) -> (String, DbType) -> ([s] -> [s])
flatten escape (fname, typ) acc = flattenP escape Nothing (fname, typ) acc

{-# SPECIALIZE flattenP :: (Utf8 -> Utf8) -> Maybe Utf8 -> (String, DbType) -> ([Utf8] -> [Utf8]) #-}
flattenP :: StringLike s => (s -> s) -> Maybe s -> (String, DbType) -> ([s] -> [s])
flattenP escape prefix (fname, typ) acc = go typ where
  go typ' = case typ' of
    DbEmbedded emb _ -> handleEmb emb
    _            -> escape fullName : acc
  fullName = case prefix of
    Nothing -> fromString fname
    Just prefix -> prefix <> fromChar delim <> fromString fname
  handleEmb (EmbeddedDef False ts) = foldr (flattenP escape (Just fullName)) acc ts
  handleEmb (EmbeddedDef True  ts) = foldr (flattenP escape Nothing) acc ts

commasJoin :: StringLike s => [s] -> s
commasJoin = intercalateS (fromChar ',')

{-# INLINEABLE intercalateS #-}
intercalateS :: StringLike s => s -> [s] -> s
intercalateS _ [] = mempty
intercalateS a (x:xs) = x <> go xs where
  go [] = mempty
  go (f:fs) = a <> f <> go fs

{-# INLINABLE renderUpdates #-}
renderUpdates :: SqlDb db => RenderConfig -> [Update db r] -> Maybe (RenderS db r)
renderUpdates conf upds = (case mapMaybe go upds of
  [] -> Nothing
  xs -> Just $ commasJoin xs) where
  go (Update field expr) = guard $ commasJoin $ zipWith (\f1 f2 -> f1 <> fromChar '=' <> f2) fs (rend expr) where
    rend = renderExprExtended conf 0
    fs = concatMap rend (projectionExprs field [])
    guard a = if null fs then Nothing else Just a

-- | Returns escaped table name optionally qualified with schema
{-# SPECIALIZE tableName :: (Utf8 -> Utf8) -> EntityDef -> ConstructorDef -> Utf8 #-}
tableName :: StringLike s => (s -> s) -> EntityDef -> ConstructorDef -> s
tableName esc e c = qualifySchema esc e tName where
  tName = esc $ if isSimple (constructors e)
    then fromString $ entityName e
    else fromString (entityName e) <> fromChar delim <> fromString (constrName c)

-- | Returns escaped main table name optionally qualified with schema
{-# SPECIALIZE mainTableName :: (Utf8 -> Utf8) -> EntityDef -> Utf8 #-}
mainTableName :: StringLike s => (s -> s) -> EntityDef -> s
mainTableName esc e = qualifySchema esc e tName where
  tName = esc $ fromString $ entityName e

{-# SPECIALIZE qualifySchema :: (Utf8 -> Utf8) -> EntityDef -> Utf8 -> Utf8 #-}
qualifySchema :: StringLike s => (s -> s) -> EntityDef -> s -> s
qualifySchema esc e name = maybe name (\sch -> esc (fromString sch) <> fromChar '.' <> name) $ entitySchema e
