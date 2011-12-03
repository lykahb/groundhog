{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell #-}
import Database.Groundhog.Core
import qualified Database.Groundhog.Generic.Sql.String as S1
import qualified Database.Groundhog.Generic.Sql.Utf8 as S2
import Database.Groundhog.TH
import Criterion.Main
import Data.String

data SomeData = SomeData {
    intField :: Int
  , stringField :: String
  }
  
deriveEntity ''SomeData Nothing

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

main = print (S1.fromStringS $ S1.renderOrders id [Asc IntFieldField, Desc IntFieldField]) >> defaultMain [
        bgroup "sqlarith"   [ bench "renderArithString" $ whnf (length . S1.fromStringS . S1.getQuery . S1.renderArith id) arithExpr
                            , bench "renderArithUtf8" $ whnf (S2.fromUtf8 . S2.getQuery . S2.renderArith id) arithExpr
                            ]
      , bgroup "sqlcond"    [ bench "renderCondString" $ whnf (length . S1.fromStringS . S1.getQuery . S1.renderCond id (fromString S1.defId) S1.defRenderEquals S1.defRenderNotEquals) condExpr
                            , bench "renderCondUtf8" $ whnf (S2.fromUtf8 . S2.getQuery . S2.renderCond id (fromString S2.defId) S2.defRenderEquals S2.defRenderNotEquals) condExpr
                            ]
      , bgroup "sqlorder"   [ bench "renderOrderString" $ whnf (length . S1.fromStringS . S1.renderOrders id) orders
                            , bench "renderOrderUtf8" $ whnf (S2.fromUtf8 . S2.renderOrders id) orders
                            ]
      , bgroup "sqlupdates" [ bench "renderUpdatesString" $ whnf (length . S1.fromStringS . S1.getQuery . S1.renderUpdates id) updates
                            , bench "renderUpdatesUtf8" $ whnf (S2.fromUtf8 . S2.getQuery . S2.renderUpdates id) updates
                            ]
                    ]
