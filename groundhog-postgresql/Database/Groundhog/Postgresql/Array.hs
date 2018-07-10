{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, OverloadedStrings, UndecidableInstances, OverlappingInstances, BangPatterns #-}

-- | See detailed documentation for PostgreSQL arrays at http://www.postgresql.org/docs/9.2/static/arrays.html and http://www.postgresql.org/docs/9.2/static/functions-array.html
module Database.Groundhog.Postgresql.Array
  (
    Array(..)
  , (!)
  , (!:)
  , append
  , prepend
  , arrayCat
  , arrayDims
  , arrayNDims
  , arrayLower
  , arrayUpper
  , arrayLength
  , arrayToString
  , stringToArray
  , any
  , all
  , (@>)
  , (<@)
  , overlaps
  ) where

import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql hiding (append)
import Database.Groundhog.Postgresql hiding (append)

import Blaze.ByteString.Builder (fromByteString, toByteString)
import Blaze.ByteString.Builder.Word (fromWord8)
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Zepto as Z
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Monoid hiding ((<>))
import Data.Word
import qualified Data.Vector as V
import Data.Traversable (traverse)
import Prelude hiding (all, any)

-- | Represents PostgreSQL arrays
newtype Array a = Array [a] deriving (Eq, Show)

instance A.ToJSON a => A.ToJSON (Array a) where
  toJSON (Array xs) = A.toJSON xs

instance A.FromJSON a => A.FromJSON (Array a) where
  parseJSON (A.Array xs) = fmap (Array . V.toList) (traverse A.parseJSON xs)
  parseJSON _            = mzero

instance (ArrayElem a, PrimitivePersistField a) => PersistField (Array a) where
  persistName a = "Array" ++ delim : persistName ((undefined :: Array a -> a) a)
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType p a = DbTypePrimitive (arrayType p a) False Nothing Nothing

arrayType :: (DbDescriptor db, ArrayElem a, PrimitivePersistField a) => proxy db -> Array a -> DbTypePrimitive
arrayType p a = DbOther $ OtherTypeDef $ [Right elemType, Left "[]"] where
  elemType = case dbType p ((undefined :: Array a -> a) a) of
    DbTypePrimitive t _ _ _ -> t
    t -> error $ "arrayType " ++ persistName a ++ ": expected DbTypePrimitive, got " ++ show t

class ArrayElem a where
  parseElem :: Parser a

instance ArrayElem a => ArrayElem (Array a) where
  parseElem = parseArr

instance PrimitivePersistField a => ArrayElem a where
  parseElem = fmap (fromPrimitivePersistValue . PersistByteString) parseString

instance (ArrayElem a, PrimitivePersistField a) => PrimitivePersistField (Array a) where
  toPrimitivePersistValue (Array xs) = PersistCustom arr (vals []) where
    arr = "ARRAY[" <> query <> "]::" <> fromString typ
    RenderS query vals = commasJoin $ map (renderPersistValue . toPrimitivePersistValue) xs
    typ = showSqlType $ arrayType (undefined :: p Postgresql) $ Array xs
  fromPrimitivePersistValue a = parseHelper parser a where
    dimensions = char '[' *> takeWhile1 (/= '=') *> char '='
    parser = optional dimensions *> parseArr

parseString :: Parser ByteString
parseString = (char '"' *> jstring_)
          <|> takeWhile1 (\c -> c /= ',' && c /= '}')
          
-- Borrowed from aeson
jstring_ :: Parser ByteString
jstring_ = {-# SCC "jstring_" #-} do
  s <- A.scan False $ \s c -> if s then Just False
                                   else if c == doubleQuote
                                        then Nothing
                                        else Just (c == backslash)
  _ <- A.word8 doubleQuote
  if backslash `B.elem` s
    then case Z.parse unescape s of
           Right r  -> return r
           Left err -> fail err
    else return s
{-# INLINE jstring_ #-}

-- Borrowed from aeson
unescape :: Z.Parser ByteString
unescape = toByteString <$> go mempty where
  go acc = do
    h <- Z.takeWhile (/=backslash)
    let rest = do
          start <- Z.take 2
          let !slash = B.unsafeHead start
              !t = B.unsafeIndex start 1
              escape = if t == doubleQuote || t == backslash
                then t
                else 255
          if slash /= backslash || escape == 255
            then fail "invalid array escape sequence"
            else do
            let cont m = go (acc `mappend` fromByteString h `mappend` m)
                {-# INLINE cont #-}
            cont (fromWord8 escape)
    done <- Z.atEnd
    if done
      then return (acc `mappend` fromByteString h)
      else rest

doubleQuote, backslash :: Word8
doubleQuote = 34
backslash = 92
  
parseArr :: ArrayElem a => Parser (Array a)
parseArr = Array <$> (char '{' *> parseElem `sepBy` char ',' <* char '}')

(!) :: (ExpressionOf Postgresql r a (Array elem), ExpressionOf Postgresql r b Int) => a -> b -> Expr Postgresql r elem
(!) arr i = mkExpr $ Snippet $ \conf _ -> [renderExpr conf (toExpr arr) <> "[" <> renderExpr conf (toExpr i) <> "]"]

(!:) :: (ExpressionOf Postgresql r a (Array elem), ExpressionOf Postgresql r i1 Int, ExpressionOf Postgresql r i2 Int) => a -> (i1, i2) -> Expr Postgresql r (Array elem)
(!:) arr (i1, i2) = mkExpr $ Snippet $ \conf _ -> [renderExpr conf (toExpr arr) <> "[" <> renderExpr conf (toExpr i1) <> ":" <> renderExpr conf (toExpr i2) <> "]"]

prepend :: (ExpressionOf Postgresql r a elem, ExpressionOf Postgresql r b (Array elem)) => a -> b -> Expr Postgresql r (Array elem)
prepend a b = mkExpr $ function "array_prepend" [toExpr a, toExpr b]

append :: (ExpressionOf Postgresql r a (Array elem), ExpressionOf Postgresql r b elem) => a -> b -> Expr Postgresql r (Array elem)
append a b = mkExpr $ function "array_append" [toExpr a, toExpr b]

arrayCat :: (ExpressionOf Postgresql r a (Array elem), ExpressionOf Postgresql r b (Array elem)) => a -> b -> Expr Postgresql r (Array elem)
arrayCat a b = mkExpr $ function "array_cat" [toExpr a, toExpr b]

arrayDims :: (ExpressionOf Postgresql r a (Array elem)) => a -> Expr Postgresql r String
arrayDims arr = mkExpr $ function "array_dims" [toExpr arr]

arrayNDims :: (ExpressionOf Postgresql r a (Array elem)) => a -> Expr Postgresql r Int
arrayNDims arr = mkExpr $ function "array_ndims" [toExpr arr]

arrayLower :: (ExpressionOf Postgresql r a (Array elem)) => a -> Int -> Expr Postgresql r Int
arrayLower arr dim = mkExpr $ function "array_lower" [toExpr arr, toExpr dim]

arrayUpper :: (ExpressionOf Postgresql r a (Array elem)) => a -> Int -> Expr Postgresql r Int
arrayUpper arr dim = mkExpr $ function "array_upper" [toExpr arr, toExpr dim]

arrayLength :: (ExpressionOf Postgresql r a (Array elem)) => a -> Int -> Expr Postgresql r Int
arrayLength arr dim = mkExpr $ function "array_length" [toExpr arr, toExpr dim]

-- | Concatenates array elements using supplied delimiter. array_to_string(ARRAY[1, 2, 3], '~^~') = 1~^~2~^~3
arrayToString :: (ExpressionOf Postgresql r a (Array elem)) => a -> String -> Expr Postgresql r String
arrayToString arr sep = mkExpr $ function "array_to_string" [toExpr arr, toExpr sep]

-- | Splits string into array elements using supplied delimiter. string_to_array('xx~^~yy~^~zz', '~^~') = {xx,yy,zz}
stringToArray :: (ExpressionOf Postgresql r a String) => a -> String -> Expr Postgresql r (Array String)
stringToArray arr sep = mkExpr $ function "string_to_array" [toExpr arr, toExpr sep]

any :: (ExpressionOf Postgresql r a elem, ExpressionOf Postgresql r b (Array elem)) => a -> b -> Cond Postgresql r
any a arr = CondRaw $ Snippet $ \conf _ -> [renderExprPriority conf 37 (toExpr a) <> "=ANY" <> fromChar '(' <> renderExpr conf (toExpr arr) <> fromChar ')']

all :: (ExpressionOf Postgresql r a elem, ExpressionOf Postgresql r b (Array elem)) => a -> b -> Cond Postgresql r
all a arr = CondRaw $ Snippet $ \conf _ -> [renderExprPriority conf 37 (toExpr a) <> "=ALL" <> fromChar '(' <> renderExpr conf (toExpr arr) <> fromChar ')']

-- | Contains. ARRAY[1,4,3] \@> ARRAY[3,1] = t
(@>) :: (ExpressionOf Postgresql r a (Array elem), ExpressionOf Postgresql r b (Array elem)) => a -> b -> Cond Postgresql r
(@>) a b = CondRaw $ operator 50 "@>" a b

-- | Is contained by. ARRAY[2,7] <\@ ARRAY[1,7,4,2,6] = t
(<@) :: (ExpressionOf Postgresql r a (Array elem), ExpressionOf Postgresql r b (Array elem)) => a -> b -> Cond Postgresql r
(<@) a b = CondRaw $ operator 50 "<@" a b

-- | Overlap (have elements in common). ARRAY[1,4,3] && ARRAY[2,1] = t
overlaps :: (ExpressionOf Postgresql r a (Array elem), ExpressionOf Postgresql r b (Array elem)) => a -> b -> Cond Postgresql r
overlaps a b = CondRaw $ operator 50 "&&" a b

parseHelper :: Parser a -> PersistValue -> a
parseHelper p (PersistByteString bs) = either error id $ parseOnly p bs
parseHelper _ a = error $ "parseHelper: expected PersistByteString, got " ++ show a
