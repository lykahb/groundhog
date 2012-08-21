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

{-# SPECIALIZE renderArith :: (PersistEntity v, Constructor c, DbDescriptor db) => Proxy db -> (Utf8 -> Utf8) -> Arith v c a -> RenderS Utf8 #-}

{-# SPECIALIZE renderCond :: (PersistEntity v, Constructor c, DbDescriptor db)
  => Proxy db
  -> (Utf8 -> Utf8)
  -> (Utf8 -> Utf8 -> Utf8)
  -> (Utf8 -> Utf8 -> Utf8)
  -> Cond v c -> Maybe (RenderS Utf8) #-}

{-# SPECIALIZE renderOrders :: (PersistEntity v, Constructor c) => (Utf8 -> Utf8) -> [Order v c] -> Utf8 #-}

{-# SPECIALIZE renderUpdates :: (PersistEntity v, Constructor c, DbDescriptor db) => Proxy db -> (Utf8 -> Utf8) -> [Update v c] -> Maybe (RenderS Utf8) #-}

{-# SPECIALIZE renderFields :: (Utf8 -> Utf8) -> [(String, DbType)] -> Utf8 #-}

{-# SPECIALIZE renderChain :: (Utf8 -> Utf8) -> FieldChain -> [Utf8] -> [Utf8] #-}
