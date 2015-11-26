{-# LANGUAGE GADTs, OverloadedStrings, FlexibleContexts #-}
-- | See detailed documentation for PostgreSQL HStore at http://www.postgresql.org/docs/9.3/static/hstore.html
module Database.Groundhog.Postgresql.HStore
  ( -- * HStore manipulation
    HStoreList(..)
  , HStoreMap(..)
  , (->.)
  , lookupArr
  , hstoreConcat
  , deleteKey
  , deleteKeys
  , difference
  , hstore_to_array
  , hstore_to_matrix
  , akeys
  , avals
  , slice
  , hstore_to_json
  , hstore_to_json_loose
  -- * HStore conditions
  , exist
  , defined
  , (?&)
  , (?|)
  , (@>)
  , (<@)
  ) where

import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Postgresql
import Database.Groundhog.Postgresql.Array (Array)

import Database.PostgreSQL.Simple.HStore

import Data.Aeson (Value)
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char8 as B
import Control.Applicative
import qualified Data.Map as Map
import Data.String

import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P (isSpace_w8)
import           Data.ByteString.Internal (c2w, w2c)
import           Data.Text(Text)
import qualified Data.Text               as TS
import qualified Data.Text.Encoding      as TS
import           Data.Text.Encoding.Error(UnicodeException)

instance PersistField HStoreList where
  persistName _ = "HStoreList"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef $ [Left "hstore"]) False Nothing Nothing

instance PersistField HStoreMap where
  persistName _ = "HStoreMap"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef $ [Left "hstore"]) False Nothing Nothing

instance PrimitivePersistField HStoreList where
  toPrimitivePersistValue a = toPrimitivePersistValue $ B.toLazyByteString $ B.fromChar '\'' <> toBuilder (toHStore a) <> B.fromString "'::hstore"
  fromPrimitivePersistValue x = case P.parseOnly (parseHStore <* P.endOfInput) $ fromPrimitivePersistValue x of
     Left err -> error $ "HStoreList: " ++ err
     Right (Left err) -> error $ "HStoreList: " ++ show err
     Right (Right val) -> val

instance PrimitivePersistField HStoreMap where
  toPrimitivePersistValue a = toPrimitivePersistValue $ B.toLazyByteString $ B.fromChar '\'' <> toBuilder (toHStore a) <> B.fromString "'::hstore"
  fromPrimitivePersistValue x = case fromPrimitivePersistValue x of
    HStoreList xs -> HStoreMap $ Map.fromList xs

-- remove parsing later
parseHStore :: P.Parser (Either UnicodeException HStoreList)
parseHStore = do
    kvs <- P.sepBy' (skipWhiteSpace *> parseHStoreKeyVal)
                    (skipWhiteSpace *> P.word8 (c2w ','))
    return $ HStoreList <$> sequence kvs

parseHStoreKeyVal :: P.Parser (Either UnicodeException (Text,Text))
parseHStoreKeyVal = do
  mkey <- parseHStoreText
  case mkey of
    Left err -> return (Left err)
    Right key -> do
      skipWhiteSpace
      _ <- P.string "=>"
      skipWhiteSpace
      mval <- parseHStoreText
      case mval of
        Left  err -> return (Left err)
        Right val -> return (Right (key,val))

skipWhiteSpace :: P.Parser ()
skipWhiteSpace = P.skipWhile P.isSpace_w8

parseHStoreText :: P.Parser (Either UnicodeException Text)
parseHStoreText = do
  _ <- P.word8 (c2w '"')
  mtexts <- parseHStoreTexts id
  case mtexts of
    Left  err   -> return (Left err)
    Right texts -> do
                     _ <- P.word8 (c2w '"')
                     return (Right (TS.concat texts))

parseHStoreTexts :: ([Text] -> [Text])
                 -> P.Parser (Either UnicodeException [Text])
parseHStoreTexts acc = do
  mchunk <- TS.decodeUtf8' <$> P.takeWhile (not . isSpecialChar)
  case mchunk of
    Left err    -> return (Left err)
    Right chunk ->
        (do
          _ <- P.word8 (c2w '\\')
          c <- TS.singleton . w2c <$> P.satisfy isSpecialChar
          parseHStoreTexts (acc . (chunk:) . (c:))
        ) <|> return (Right (acc [chunk]))
 where
   isSpecialChar c = c == c2w '\\' || c == c2w '"'


----------------------------------------------------------------------

psqlOperatorExpr :: (db ~ Postgresql, Expression db r a, Expression db r b) => String -> a -> b -> Expr db r c
psqlOperatorExpr op x y = mkExpr $ operator 50 op x y

psqlOperatorCond :: (db ~ Postgresql, Expression db r a, Expression db r b) => String -> a -> b -> Cond db r
psqlOperatorCond op x y = CondRaw $ operator 50 op x y

-- | Get value for key (NULL if not present)
-- 
-- @'a=>x, b=>y'::hstore -> 'a' == x@
(->.) :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ExpressionOf db r key key', ToHStore hstore', IsString key')
      => hstore -> key -> Expr db r (Maybe Text)
(->.) = psqlOperatorExpr "->"

-- | Get values for keys array (NULL if not present)
-- 
-- @'a=>x, b=>y, c=>z'::hstore == ARRAY['c','a']  {"z","x"}@
lookupArr :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ExpressionOf db r keys (Array Text), ToHStore hstore')
          => hstore -> keys -> Expr db r (Array Text)
lookupArr = psqlOperatorExpr "->"

-- | Concatenate hstores
-- 
-- @'a=>b, c=>d'::hstore || 'c=>x, d=>q'::hstore == "a"=>"b", "c"=>"x", "d"=>"q"@
hstoreConcat :: (db ~ Postgresql, ExpressionOf db r hstore1 hstore1', ExpressionOf db r hstore2 hstore2', ToHStore hstore1', ToHStore hstore2', ToHStore hstore)
             => hstore1 -> hstore2 -> Expr db r hstore
hstoreConcat = psqlOperatorExpr "||"

-- | Does hstore contain key? Same as postgresql operator ?.
-- 
-- @'a=>1'::hstore ? 'a' == True@
exist :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ExpressionOf db r key key', ToHStore hstore', IsString key')
      => hstore -> key -> Cond db r
exist h k = CondRaw $ function "delete" [toExpr h, toExpr k]

-- | Does hstore contain non-NULL value for key?
-- 
-- @defined('a=>NULL','a') == f@
defined :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ExpressionOf db r key key', ToHStore hstore', IsString key')
      => hstore -> key -> Cond db r
defined h k = CondRaw $ function "defined" [toExpr h, toExpr k]

-- | Does hstore contain all specified keys?
-- 
-- @'a=>1,b=>2'::hstore ?& ARRAY['a','b'] == True@
(?&) :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ExpressionOf db r keys (Array Text), ToHStore hstore')
     => hstore -> keys -> Cond db r
(?&) = psqlOperatorCond "?&"

-- | Does hstore contain any of the specified keys?
-- 
-- @'a=>1,b=>2'::hstore ?| ARRAY['b','c'] == True@
(?|) :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ExpressionOf db r keys (Array Text), ToHStore hstore')
     => hstore -> keys -> Cond db r
(?|) = psqlOperatorCond "?|"

-- | Does left operand contain right?
-- 
-- @'a=>b, b=>1, c=>NULL'::hstore @> 'b=>1' == True@
(@>) :: (db ~ Postgresql, ExpressionOf db r hstore1 hstore1', ExpressionOf db r hstore2 hstore2', ToHStore hstore1', ToHStore hstore2')
     => hstore1 -> hstore2 -> Cond db r
(@>) = psqlOperatorCond "@>"

-- | Is left operand contained in right?
-- 
-- @'a=>c'::hstore <@ 'a=>b, b=>1, c=>NULL' == False@
(<@) :: (db ~ Postgresql, ExpressionOf db r hstore1 hstore1', ExpressionOf db r hstore2 hstore2', ToHStore hstore1', ToHStore hstore2')
     => hstore1 -> hstore2 -> Cond db r
(<@) = psqlOperatorCond "<@"

-- | Delete key from left operand
-- 
-- @'a=>1, b=>2, c=>3'::hstore - 'b'::text == "a"=>"1", "c"=>"3"@
deleteKey :: (db ~ Postgresql, ExpressionOf db r hstore1 hstore1', ExpressionOf db r key key', ToHStore hstore1', ToHStore hstore, IsString key')
          => hstore1 -> key -> Expr db r hstore
deleteKey h k = mkExpr $ function "delete" [toExpr h, toExpr k]

-- | Delete keys from left operand
-- 
-- @'a=>1, b=>2, c=>3'::hstore - ARRAY['a','b'] == "c"=>"3"@
deleteKeys :: (db ~ Postgresql, ExpressionOf db r hstore1 hstore1', ExpressionOf db r keys (Array Text), ToHStore hstore1', ToHStore hstore)
           => hstore1 -> keys -> Expr db r hstore
deleteKeys h k = mkExpr $ function "delete" [toExpr h, toExpr k]

-- | Delete matching pairs from left operand
-- 
-- @'a=>1, b=>2, c=>3'::hstore - 'a=>4, b=>2'::hstore == "a"=>"1", "c"=>"3"@
difference :: (db ~ Postgresql, ExpressionOf db r hstore1 hstore1', ExpressionOf db r hstore2 hstore2', ToHStore hstore1', ToHStore hstore2', ToHStore hstore)
           => hstore1 -> hstore2 -> Expr db r hstore
difference h1 h2 = mkExpr $ function "delete" [toExpr h1, toExpr h2]

-- | Convert hstore to array of alternating keys and values. Same as prefix operator %%.
-- 
-- @hstore_to_array('a=>1,b=>2') == {a,1,b,2}@
hstore_to_array :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ToHStore hstore')
                => hstore -> Expr db r (Array Text)
hstore_to_array h = mkExpr $ function "hstore_to_array" [toExpr h]

-- | Convert hstore to two-dimensional key/value array. Same as prefix operator %#.
-- 
-- @hstore_to_matrix('a=>1,b=>2') == {{a,1},{b,2}}@
hstore_to_matrix :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ToHStore hstore')
                 => hstore -> Expr db r (Array (Array Text))
hstore_to_matrix h = mkExpr $ function "hstore_to_matrix" [toExpr h]

-- | Get hstore's keys as an array
-- 
-- @akeys('a=>1,b=>2') == {a,b}@
akeys :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ToHStore hstore')
          => hstore -> Expr db r (Array Text)
akeys h = mkExpr $ function "akeys" [toExpr h]

-- | Get hstore's values as an array
-- 
-- @avals('a=>1,b=>2') == {1,2}@
avals :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ToHStore hstore')
          => hstore -> Expr db r (Array Text)
avals h = mkExpr $ function "vals" [toExpr h]

-- | Get hstore as a json value
-- 
-- @hstore_to_json('"a key"=>1, b=>t, c=>null, d=>12345, e=>012345, f=>1.234, g=>2.345e+4') 
-- == {"a key": "1", "b": "t", "c": null, "d": "12345", "e": "012345", "f": "1.234", "g": "2.345e+4"}@
hstore_to_json :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ToHStore hstore')
               => hstore -> Expr db r Value
hstore_to_json h = mkExpr $ function "hstore_to_json" [toExpr h]

-- | Get hstore as a json value, but attempting to distinguish numerical and Boolean values so they are unquoted in the JSON
-- 
-- @hstore_to_json_loose('"a key"=>1, b=>t, c=>null, d=>12345, e=>012345, f=>1.234, g=>2.345e+4')
-- == {"a key": 1, "b": true, "c": null, "d": 12345, "e": "012345", "f": 1.234, "g": 2.345e+4}@
hstore_to_json_loose :: (db ~ Postgresql, ExpressionOf db r hstore hstore', ToHStore hstore')
                     => hstore -> Expr db r Value
hstore_to_json_loose h = mkExpr $ function "hstore_to_json_loose" [toExpr h]

-- | Extract a subset of an hstore
-- 
-- @slice('a=>1,b=>2,c=>3'::hstore, ARRAY['b','c','x']) =="b"=>"2", "c"=>"3"@
slice :: (db ~ Postgresql, ExpressionOf db r hstore1 hstore1', ExpressionOf db r keys (Array Text), ToHStore hstore1', ToHStore hstore)
      => hstore1 -> keys -> Expr db r hstore
slice h k = mkExpr $ function "slice" [toExpr h, toExpr k]