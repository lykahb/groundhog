{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module Main where

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql
--import qualified Database.Groundhog.Generic.Sql.String as S1
--import qualified Database.Groundhog.Generic.Sql.Utf8 as S2
import Database.Groundhog.TH
import Criterion.Main
import Data.Maybe
import Data.Monoid
import Data.String

data SomeData = SomeData {
    intField :: Int
  , stringField :: String
  }

data Bullshit a = Bullshit (a, Int) a Int

mkPersist suffixNamingStyle [groundhog|
- entity: Bullshit
- entity: SomeData
|]
------------------------------------------------------------------------------------------
newtype StringS = StringS { fromStringS :: ShowS }

instance Monoid StringS where
  mempty = StringS id
  (StringS s1) `mappend` (StringS s2) = StringS (s1 . s2)

instance IsString StringS where
  fromString s = StringS (s++)

instance StringLike StringS where
  fromChar c = StringS (c:)

flatten :: StringLike s => (s -> s) -> (String, DbType) -> ([s] -> [s])
flatten esc (fname, typ) acc = case typ of
  DbEmbedded emb -> case emb of
    EmbeddedDef False ts -> foldr (flattenP esc (fromString fname)) acc ts
    EmbeddedDef True  ts -> foldr (flatten esc) acc ts
  _            -> esc (fromString fname) : acc

flattenP :: StringLike s => (s -> s) -> s -> (String, DbType) -> ([s] -> [s])
flattenP esc prefix (fname, typ) acc = (case typ of
  DbEmbedded emb -> case emb of
    EmbeddedDef False ts -> foldr (flattenP esc fullName) acc ts
    EmbeddedDef True  ts -> foldr (flatten esc) acc ts
  _            -> esc fullName : acc) where
    fullName = prefix <> fromChar '$' <> fromString fname

commasJoin :: StringLike s => [s] -> s
commasJoin = intercalate (fromChar ',')

{-# INLINE intercalate #-}
intercalate :: StringLike s => s -> [s] -> s
intercalate _ [] = mempty
intercalate a (x:xs) = x <> go xs where
  go [] = mempty
  go (f:fs) = a <> f <> go fs

type FieldChain1 = Either String [(String, DbType)]

renderField1 :: (StringLike s) => (s -> s) -> FieldChain1 -> [s] -> [s]
renderField1 esc field acc = case field of
  Left f -> fromString f:acc
  Right [f] -> flatten esc f acc
  Right (f:fs) -> maybe (flatten esc f acc) (\prefix -> flattenP esc prefix f acc) $ mkPrefix1 fs
  Right [] -> error "renderField1: empty field list"
  
mkPrefix1 :: StringLike s => [(String, DbType)] -> Maybe s
mkPrefix1 = go where
  go [] = Nothing
  go ((name, typ):fs) = case typ of
    DbEmbedded emb -> case emb of
      EmbeddedDef True _ -> Nothing
      EmbeddedDef False _ -> Just $ goP (fromString name) fs
    other -> error $ "mkPrefix: unexpected type " ++ show other
  goP acc [] = acc
  goP acc ((name, typ):fs) = case typ of
    DbEmbedded emb -> case emb of
      EmbeddedDef True  _ -> acc
      EmbeddedDef False _ -> goP (fromString name <> fromChar '$' <> acc) fs
    other -> error $ "mkPrefix: unexpected type " ++ show other

type FieldChain2 = ((String, DbType), [(String, EmbeddedDef)])
    
renderField2 :: (StringLike s) => (s -> s) -> FieldChain2 -> [s] -> [s]
renderField2 esc (f, prefix) acc = (case prefix of
  ((name, EmbeddedDef False _):fs) -> flattenP esc (goP (fromString name) fs) f acc
  _ -> flatten esc f acc ) where
  goP acc [] = acc
  goP acc ((name, typ):fs) = case typ of
    EmbeddedDef True  _ -> acc
    EmbeddedDef False _ -> goP (fromString name <> fromChar '$' <> acc) fs
  
------------------------------------------------------------------------------------------
  
arithExpr :: Arith SomeData SomeDataConstructor Int
arithExpr = go 10 where
  go 0 = ArithField IntFieldField
  go n = Plus (Abs (go (n - 1))) (Abs (go (n - 1)))

condExpr :: Cond SomeData SomeDataConstructor
condExpr = go 10 where
  go 0 = IntFieldField ==. 1 + 1 + 1 + toArith IntFieldField
  go n | odd n     = go (n - 1) ||. go (n - 1)
       | otherwise = go (n - 1) &&. go (n - 1)

orders :: [Order SomeData SomeDataConstructor]
orders = replicate 100 (Desc IntFieldField)

updates :: [Update SomeData SomeDataConstructor]
updates = replicate 100 (IntFieldField =. (0 :: Int))

bigdata :: DbType
bigdata = dbType (((((s, s), (s, s)), ((s, s), (s, s))), (((s, s), (s, s)), ((s, s), (s, s)))), ((((s, s), (s, s)), ((s, s), (s, s))), (((s, s), (s, s)), ((s, s), (s, s))))) where
  s = "" :: String

columns :: [(String, DbType)]
columns = zip (repeat "abc") $ replicate 100 bigdata

chain1simple :: FieldChain1
chain1simple = Left "abc"

chain1complex :: FieldChain1
chain1complex = Right [("abc", DbString)]

chain1prefix :: FieldChain1
chain1prefix = Right [("abc", DbString), ("abc", DbEmbedded $ EmbeddedDef True [])]

chain2simple :: FieldChain2
chain2simple = (("abc", DbString), [])

chain2prefix :: FieldChain2
chain2prefix = (("abc", DbString), [("abc", EmbeddedDef True [])])

fromStringS' :: StringS -> String
fromStringS' = ($ "") . fromStringS

main = defaultMain [
  bgroup "renderChain1" [ bench "chain1simple" $ nf (map fromStringS' . ($ []) . renderField1 id) chain1simple 
                        , bench "chain1complex" $ nf (map fromStringS' . ($ []) . renderField1 id) chain1complex 
                        , bench "chain1prefix" $ nf (map fromStringS' . ($ []) . renderField1 id) chain1prefix 
                        ],
  bgroup "renderChain2" [ bench "chain2simple" $ nf (map fromStringS' . ($ []) . renderField2 id) chain2simple 
                        , bench "chain2prefix" $ nf (map fromStringS' . ($ []) . renderField2 id) chain2prefix
                        ]
{-       bgroup "renderUpdates" [ bench "String" $ nf (length . ($ "") . S1.fromStringS . getQuery . fromJust . S1.renderUpdates id) updates
                            , bench "Utf8" $ whnf (S2.fromUtf8 . getQuery . fromJust . S2.renderUpdates id) updates
                            ]
       bgroup "renderField" [ bench "renderFieldString" $ nf (length . ($ "") . S1.fromStringS . inter . map (S1.renderField id)) columns
                            , bench "renderFieldUtf8" $ whnf (S2.fromUtf8 . inter . map (S2.renderField id)) columns
                            ]
      , bgroup "renderFields" [ bench "renderFieldStrings" $ nf (length . ($ "") . S1.fromStringS . S1.renderFields id) columns -- 2.42 ms
                            , bench "renderFieldsUtf8" $ whnf (S2.fromUtf8 . S2.renderFields id) columns  -- 2.01 ms
                            ]
      , bgroup "sqlarith"   [ bench "renderArithString" $ whnf (length . ($ "") . S1.fromStringS . S1.getQuery . S1.renderArith id) arithExpr
                            , bench "renderArithUtf8" $ whnf (S2.fromUtf8 . S2.getQuery . S2.renderArith id) arithExpr
                            ]
      , bgroup "sqlcond"    [ bench "renderCondString" $ whnf (length . ($ "") . S1.fromStringS . S1.getQuery . S1.renderCond id (fromString S1.defId) S1.defRenderEquals S1.defRenderNotEquals) condExpr
                            , bench "renderCondUtf8" $ whnf (S2.fromUtf8 . S2.getQuery . S2.renderCond id (fromString S2.defId) S2.defRenderEquals S2.defRenderNotEquals) condExpr
                            ]
      , bgroup "sqlorder"   [ bench "renderOrderString" $ whnf (length . ($ "") . S1.fromStringS . S1.renderOrders id) orders
                            , bench "renderOrderUtf8" $ whnf (S2.fromUtf8 . S2.renderOrders id) orders
                            ]
      , bgroup "sqlupdates" [ bench "renderUpdatesString" $ whnf (length . ($ "") . S1.fromStringS . S1.getQuery . S1.renderUpdates id) updates
                            , bench "renderUpdatesUtf8" $ whnf (S2.fromUtf8 . S2.getQuery . S2.renderUpdates id) updates
                            ]
-}
                    ]
