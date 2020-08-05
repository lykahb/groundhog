{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | See detailed documentation for PostgreSQL HStore at http://www.postgresql.org/docs/9.3/static/hstore.html
module Database.Groundhog.Postgresql.HStore
  ( -- * HStore manipulation
    HStore (..),
    (->.),
    lookupArr,
    hstoreConcat,
    deleteKey,
    deleteKeys,
    difference,
    hstore_to_array,
    hstore_to_matrix,
    akeys,
    avals,
    slice,
    hstore_to_json,
    hstore_to_json_loose,

    -- * HStore conditions
    exist,
    defined,
    (?&),
    (?|),
    (@>),
    (<@),
  )
where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as B (toStrict)
import qualified Data.Map as Map
import Data.String
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Postgresql
import Database.Groundhog.Postgresql.Array (Array)
import Database.PostgreSQL.Simple.HStore

newtype HStore = HStore (Map.Map Text Text)
  deriving (Eq, Ord, Show)

instance PersistField HStore where
  persistName _ = "HStore"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef [Left "hstore"]) False Nothing Nothing

instance PrimitivePersistField HStore where
  toPrimitivePersistValue (HStore a) = PersistCustom "E?::hstore" [toPrimitivePersistValue $ T.decodeUtf8 $ B.toStrict $ toLazyByteString (toHStore (HStoreMap a))]
  fromPrimitivePersistValue x = case parseHStoreList $ fromPrimitivePersistValue x of
    Left err -> error $ "HStore: " ++ err
    Right (HStoreList val) -> HStore $ Map.fromList val

----------------------------------------------------------------------

psqlOperatorExpr :: (db ~ Postgresql, Expression db r a, Expression db r b, PersistField c) => String -> a -> b -> Expr db r c
psqlOperatorExpr op x y = mkExpr $ operator 50 op x y

psqlOperatorCond :: (db ~ Postgresql, Expression db r a, Expression db r b) => String -> a -> b -> Cond db r
psqlOperatorCond op x y = CondRaw $ operator 50 op x y

-- | Get value for key (NULL if not present)
--
-- @'a=>x, b=>y'::hstore -> 'a' == x@
(->.) ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore, ExpressionOf db r key key', IsString key') =>
  hstore ->
  key ->
  Expr db r (Maybe Text)
(->.) = psqlOperatorExpr "->"

-- | Get values for keys array (NULL if not present)
--
-- @'a=>x, b=>y, c=>z'::hstore == ARRAY['c','a']  {"z","x"}@
lookupArr ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore, ExpressionOf db r keys (Array Text)) =>
  hstore ->
  keys ->
  Expr db r (Array Text)
lookupArr = psqlOperatorExpr "->"

-- | Concatenate hstores
--
-- @'a=>b, c=>d'::hstore || 'c=>x, d=>q'::hstore == "a"=>"b", "c"=>"x", "d"=>"q"@
hstoreConcat ::
  (db ~ Postgresql, ExpressionOf db r hstore1 HStore, ExpressionOf db r hstore2 HStore) =>
  hstore1 ->
  hstore2 ->
  Expr db r HStore
hstoreConcat = psqlOperatorExpr "||"

-- | Does hstore contain key? Same as postgresql operator ?.
--
-- @'a=>1'::hstore ? 'a' == True@
exist ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore, ExpressionOf db r key key', IsString key') =>
  hstore ->
  key ->
  Cond db r
exist h k = CondRaw $ function "exist" [toExpr h, toExpr k]

-- | Does hstore contain non-NULL value for key?
--
-- @defined('a=>NULL','a') == f@
defined ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore, ExpressionOf db r key key', IsString key') =>
  hstore ->
  key ->
  Cond db r
defined h k = CondRaw $ function "defined" [toExpr h, toExpr k]

-- | Does hstore contain all specified keys?
--
-- @'a=>1,b=>2'::hstore ?& ARRAY['a','b'] == True@
(?&) ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore, ExpressionOf db r keys (Array Text)) =>
  hstore ->
  keys ->
  Cond db r
(?&) = psqlOperatorCond "?&"

-- | Does hstore contain any of the specified keys?
--
-- @'a=>1,b=>2'::hstore ?| ARRAY['b','c'] == True@
(?|) ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore, ExpressionOf db r keys (Array Text)) =>
  hstore ->
  keys ->
  Cond db r
(?|) = psqlOperatorCond "?|"

-- | Does left operand contain right?
--
-- @'a=>b, b=>1, c=>NULL'::hstore @> 'b=>1' == True@
(@>) ::
  (db ~ Postgresql, ExpressionOf db r hstore1 HStore, ExpressionOf db r hstore2 HStore) =>
  hstore1 ->
  hstore2 ->
  Cond db r
(@>) = psqlOperatorCond "@>"

-- | Is left operand contained in right?
--
-- @'a=>c'::hstore <@ 'a=>b, b=>1, c=>NULL' == False@
(<@) ::
  (db ~ Postgresql, ExpressionOf db r hstore1 HStore, ExpressionOf db r hstore2 HStore) =>
  hstore1 ->
  hstore2 ->
  Cond db r
(<@) = psqlOperatorCond "<@"

-- | Delete key from left operand
--
-- @'a=>1, b=>2, c=>3'::hstore - 'b'::text == "a"=>"1", "c"=>"3"@
deleteKey ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore, ExpressionOf db r key key', IsString key') =>
  hstore ->
  key ->
  Expr db r HStore
deleteKey h k = mkExpr $ function "delete" [toExpr h, toExpr k]

-- | Delete keys from left operand
--
-- @'a=>1, b=>2, c=>3'::hstore - ARRAY['a','b'] == "c"=>"3"@
deleteKeys ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore, ExpressionOf db r keys (Array Text)) =>
  hstore ->
  keys ->
  Expr db r HStore
deleteKeys h k = mkExpr $ function "delete" [toExpr h, toExpr k]

-- | Delete matching pairs from left operand
--
-- @'a=>1, b=>2, c=>3'::hstore - 'a=>4, b=>2'::hstore == "a"=>"1", "c"=>"3"@
difference ::
  (db ~ Postgresql, ExpressionOf db r hstore1 HStore, ExpressionOf db r hstore2 HStore) =>
  hstore1 ->
  hstore2 ->
  Expr db r HStore
difference h1 h2 = mkExpr $ function "delete" [toExpr h1, toExpr h2]

-- | Convert hstore to array of alternating keys and values. Same as prefix operator %%.
--
-- @hstore_to_array('a=>1,b=>2') == {a,1,b,2}@
hstore_to_array ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore) =>
  hstore ->
  Expr db r (Array Text)
hstore_to_array h = mkExpr $ function "hstore_to_array" [toExpr h]

-- | Convert hstore to two-dimensional key/value array. Same as prefix operator %#.
--
-- @hstore_to_matrix('a=>1,b=>2') == {{a,1},{b,2}}@
hstore_to_matrix ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore) =>
  hstore ->
  Expr db r (Array (Array Text))
hstore_to_matrix h = mkExpr $ function "hstore_to_matrix" [toExpr h]

-- | Get hstore's keys as an array
--
-- @akeys('a=>1,b=>2') == {a,b}@
akeys ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore) =>
  hstore ->
  Expr db r (Array Text)
akeys h = mkExpr $ function "akeys" [toExpr h]

-- | Get hstore's values as an array
--
-- @avals('a=>1,b=>2') == {1,2}@
avals ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore) =>
  hstore ->
  Expr db r (Array Text)
avals h = mkExpr $ function "vals" [toExpr h]

-- | Get hstore as a json value
--
-- @hstore_to_json('"a key"=>1, b=>t, c=>null, d=>12345, e=>012345, f=>1.234, g=>2.345e+4')
-- == {"a key": "1", "b": "t", "c": null, "d": "12345", "e": "012345", "f": "1.234", "g": "2.345e+4"}@
hstore_to_json ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore) =>
  hstore ->
  Expr db r Value
hstore_to_json h = mkExpr $ function "hstore_to_json" [toExpr h]

-- | Get hstore as a json value, but attempting to distinguish numerical and Boolean values so they are unquoted in the JSON
--
-- @hstore_to_json_loose('"a key"=>1, b=>t, c=>null, d=>12345, e=>012345, f=>1.234, g=>2.345e+4')
-- == {"a key": 1, "b": true, "c": null, "d": 12345, "e": "012345", "f": 1.234, "g": 2.345e+4}@
hstore_to_json_loose ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore) =>
  hstore ->
  Expr db r Value
hstore_to_json_loose h = mkExpr $ function "hstore_to_json_loose" [toExpr h]

-- | Extract a subset of an hstore
--
-- @slice('a=>1,b=>2,c=>3'::hstore, ARRAY['b','c','x']) =="b"=>"2", "c"=>"3"@
slice ::
  (db ~ Postgresql, ExpressionOf db r hstore HStore, ExpressionOf db r keys (Array Text)) =>
  hstore ->
  keys ->
  Expr db r HStore
slice h k = mkExpr $ function "slice" [toExpr h, toExpr k]
