module Database.Groundhog.Generic.Sql.String
    ( module Database.Groundhog.Generic.Sql
    , StringS (..)
    ) where

import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql
import Data.Monoid
import Data.String

newtype StringS = StringS { fromStringS :: ShowS }

instance Monoid StringS where
  mempty = StringS id
  (StringS s1) `mappend` (StringS s2) = StringS (s1 . s2)

instance IsString StringS where
  fromString s = StringS (s++)

instance StringLike StringS where
  fromChar c = StringS (c:)

{-# SPECIALIZE (<>) :: RenderS StringS -> RenderS StringS -> RenderS StringS #-}

{-# SPECIALIZE renderArith :: PersistEntity v => (StringS -> StringS) -> Arith v c a -> RenderS StringS #-}

{-# SPECIALIZE renderCond :: PersistEntity v
  => (StringS -> StringS)
  -> String -- name of id in constructor table
  -> (StringS -> StringS -> StringS)
  -> (StringS -> StringS -> StringS)
  -> Cond v c -> Maybe (RenderS StringS) #-}

{-# SPECIALIZE renderOrders :: PersistEntity v => (StringS -> StringS) -> [Order v c] -> StringS #-}

{-# SPECIALIZE renderUpdates :: PersistEntity v => (StringS -> StringS) -> [Update v c] -> Maybe (RenderS StringS) #-}

{-# SPECIALIZE renderFields :: (StringS -> StringS) -> [(String, DbType)] -> StringS #-}
