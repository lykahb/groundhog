{-# LANGUAGE Rank2Types #-}
module Database.Groundhog.Generic.Sql.Utf8
    ( module Database.Groundhog.Generic.Sql
    , Utf8 (..)
    , fromUtf8
    ) where

import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql
import Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import Data.ByteString
import Data.Monoid
import Data.String

newtype Utf8 = Utf8 Builder

fromUtf8 :: Utf8 -> ByteString
fromUtf8 (Utf8 a) = toByteString a

instance Monoid Utf8 where
  mempty = Utf8 mempty
  mappend (Utf8 a) (Utf8 b) = Utf8 (mappend a b)

instance IsString Utf8 where
  fromString = Utf8 . B.fromString

instance StringLike Utf8 where
  fromChar = Utf8 . B.fromChar

{-# SPECIALIZE (<>) :: RenderS Utf8 -> RenderS Utf8 -> RenderS Utf8 #-}

{-# SPECIALIZE renderArith :: PersistEntity v => (String -> String) -> Arith v c a -> RenderS Utf8 #-}

{-# SPECIALIZE renderCond :: PersistEntity v
  => (String -> String)
  -> String -- name of id in constructor table
  -> (forall a.PersistField a => (String -> String) -> Expr v c a -> Expr v c a -> RenderS Utf8)
  -> (forall a.PersistField a => (String -> String) -> Expr v c a -> Expr v c a -> RenderS Utf8)
  -> Cond v c -> RenderS Utf8 #-}

{-# SPECIALIZE defRenderEquals :: PersistField a => (String -> String) -> Expr v c a -> Expr v c a -> RenderS Utf8 #-}
{-# SPECIALIZE defRenderNotEquals :: PersistField a => (String -> String) -> Expr v c a -> Expr v c a -> RenderS Utf8 #-}

{-# SPECIALIZE renderExpr :: (String -> String) -> Expr v c a -> RenderS Utf8 #-}

{-# SPECIALIZE renderOrders :: PersistEntity v => (String -> String) -> [Order v c] -> Utf8 #-}

{-# SPECIALIZE renderUpdates :: PersistEntity v => (String -> String) -> [Update v c] -> RenderS Utf8 #-}

{-# SPECIALIZE renderFields :: (Utf8 -> Utf8) -> [(String, NamedType)] -> Utf8 #-}
