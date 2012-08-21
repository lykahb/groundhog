{-# LANGUAGE TypeFamilies, GADTs, TypeSynonymInstances, OverlappingInstances, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Groundhog.Instances (Selector(..)) where

import Control.Monad (liftM)

import Database.Groundhog.Core
import Database.Groundhog.Generic (failMessage, primToPersistValue, primFromPersistValue)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Bits (bitSize)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Word (Word8, Word16, Word32, Word64)

instance (PersistField a', PersistField b') => Embedded (a', b') where
  data Selector (a', b') constr where
    Tuple2_0Selector :: Selector (a, b) a
    Tuple2_1Selector :: Selector (a, b) b
  selectorNum Tuple2_0Selector = 0
  selectorNum Tuple2_1Selector = 1

instance (PersistField a', PersistField b', PersistField c') => Embedded (a', b', c') where
  data Selector (a', b', c') constr where
    Tuple3_0Selector :: Selector (a, b, c) a
    Tuple3_1Selector :: Selector (a, b, c) b
    Tuple3_2Selector :: Selector (a, b, c) c
  selectorNum Tuple3_0Selector = 0
  selectorNum Tuple3_1Selector = 1
  selectorNum Tuple3_2Selector = 2

instance (PersistField a', PersistField b', PersistField c', PersistField d') => Embedded (a', b', c', d') where
  data Selector (a', b', c', d') constr where
    Tuple4_0Selector :: Selector (a, b, c, d) a
    Tuple4_1Selector :: Selector (a, b, c, d) b
    Tuple4_2Selector :: Selector (a, b, c, d) c
    Tuple4_3Selector :: Selector (a, b, c, d) d
  selectorNum Tuple4_0Selector = 0
  selectorNum Tuple4_1Selector = 1
  selectorNum Tuple4_2Selector = 2
  selectorNum Tuple4_3Selector = 3

instance (PersistField a', PersistField b', PersistField c', PersistField d', PersistField e') => Embedded (a', b', c', d', e') where
  data Selector (a', b', c', d', e') constr where
    Tuple5_0Selector :: Selector (a, b, c, d, e) a
    Tuple5_1Selector :: Selector (a, b, c, d, e) b
    Tuple5_2Selector :: Selector (a, b, c, d, e) c
    Tuple5_3Selector :: Selector (a, b, c, d, e) d
    Tuple5_4Selector :: Selector (a, b, c, d, e) e
  selectorNum Tuple5_0Selector = 0
  selectorNum Tuple5_1Selector = 1
  selectorNum Tuple5_2Selector = 2
  selectorNum Tuple5_3Selector = 3
  selectorNum Tuple5_4Selector = 4
  
instance PrimitivePersistField a => SinglePersistField a where
  toSinglePersistValue a = phantomDb >>= \p -> return (toPrim p a)
  fromSinglePersistValue a = phantomDb >>= \p -> return (fromPrim p a)

instance (SinglePersistField a, NeverNull a) => SinglePersistField (Maybe a) where
  toSinglePersistValue Nothing = return PersistNull
  toSinglePersistValue (Just a) = toSinglePersistValue a
  fromSinglePersistValue PersistNull = return Nothing
  fromSinglePersistValue a = liftM Just $ fromSinglePersistValue a

instance PrimitivePersistField a => PurePersistField a where
  toPurePersistValues p a = (toPrim p a:)
  fromPurePersistValues p (x:xs) = (fromPrim p x, xs)
  fromPurePersistValues _ xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance (PrimitivePersistField a, NeverNull a) => PurePersistField (Maybe a) where
  toPurePersistValues p a = (maybe PersistNull (toPrim p) a:)
  fromPurePersistValues _ (PersistNull:xs) = (Nothing, xs)
  fromPurePersistValues p (x:xs) = (fromPrim p x, xs)
  fromPurePersistValues _ xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField () where
  toPurePersistValues _ _ = id
  fromPurePersistValues _ xs = ((), xs)

instance (PurePersistField a, PurePersistField b) => PurePersistField (a, b) where
  toPurePersistValues p (a, b) = toPurePersistValues p a . toPurePersistValues p b
  fromPurePersistValues p xs = let
    (a, rest0) = fromPurePersistValues p xs
    (b, rest1) = fromPurePersistValues p rest0
    in ((a, b), rest1)

instance (PurePersistField a, PurePersistField b, PurePersistField c) => PurePersistField (a, b, c) where
  toPurePersistValues p (a, b, c) = toPurePersistValues p a . toPurePersistValues p b . toPurePersistValues p c
  fromPurePersistValues p xs = let
    (a, rest0) = fromPurePersistValues p xs
    (b, rest1) = fromPurePersistValues p rest0
    (c, rest2) = fromPurePersistValues p rest1
    in ((a, b, c), rest2)
  
instance (PurePersistField a, PurePersistField b, PurePersistField c, PurePersistField d) => PurePersistField (a, b, c, d) where
  toPurePersistValues p (a, b, c, d) = toPurePersistValues p a . toPurePersistValues p b . toPurePersistValues p c . toPurePersistValues p d
  fromPurePersistValues p xs = let
    (a, rest0) = fromPurePersistValues p xs
    (b, rest1) = fromPurePersistValues p rest0
    (c, rest2) = fromPurePersistValues p rest1
    (d, rest3) = fromPurePersistValues p rest2
    in ((a, b, c, d), rest3)
  
instance (PurePersistField a, PurePersistField b, PurePersistField c, PurePersistField d, PurePersistField e) => PurePersistField (a, b, c, d, e) where
  toPurePersistValues p (a, b, c, d, e) = toPurePersistValues p a . toPurePersistValues p b . toPurePersistValues p c . toPurePersistValues p d . toPurePersistValues p e
  fromPurePersistValues p xs = let
    (a, rest0) = fromPurePersistValues p xs
    (b, rest1) = fromPurePersistValues p rest0
    (c, rest2) = fromPurePersistValues p rest1
    (d, rest3) = fromPurePersistValues p rest2
    (e, rest4) = fromPurePersistValues p rest3
    in ((a, b, c, d, e), rest4)

instance Numeric Int
instance Numeric Int8
instance Numeric Int16
instance Numeric Int32
instance Numeric Int64
instance Numeric Word8
instance Numeric Word16
instance Numeric Word32
instance Numeric Word64
instance Numeric Double

instance PrimitivePersistField String where
  toPrim _ s = PersistString s
  fromPrim _ (PersistString s) = s
  fromPrim _ (PersistByteString bs) = T.unpack $ T.decodeUtf8With T.lenientDecode bs
  fromPrim _ (PersistInt64 i) = show i
  fromPrim _ (PersistDouble d) = show d
  fromPrim _ (PersistDay d) = show d
  fromPrim _ (PersistTimeOfDay d) = show d
  fromPrim _ (PersistUTCTime d) = show d
  fromPrim _ (PersistBool b) = show b
  fromPrim _ PersistNull = error "Unexpected null"

instance PrimitivePersistField T.Text where
  toPrim _ a = PersistString (T.unpack a)
  fromPrim _ (PersistByteString bs) = T.decodeUtf8With T.lenientDecode bs
  fromPrim p x = T.pack $ fromPrim p x

instance PrimitivePersistField ByteString where
  toPrim _ s = PersistByteString s
  fromPrim _ (PersistByteString a) = a
  fromPrim p x = T.encodeUtf8 . T.pack $ fromPrim p x

instance PrimitivePersistField Int where
  toPrim _ a = PersistInt64 (fromIntegral a)
  fromPrim _ (PersistInt64 a) = fromIntegral a
  fromPrim _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int8 where
  toPrim _ a = PersistInt64 (fromIntegral a)
  fromPrim _ (PersistInt64 a) = fromIntegral a
  fromPrim _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int16 where
  toPrim _ a = PersistInt64 (fromIntegral a)
  fromPrim _ (PersistInt64 a) = fromIntegral a
  fromPrim _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int32 where
  toPrim _ a = PersistInt64 (fromIntegral a)
  fromPrim _ (PersistInt64 a) = fromIntegral a
  fromPrim _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int64 where
  toPrim _ a = PersistInt64 (fromIntegral a)
  fromPrim _ (PersistInt64 a) = a
  fromPrim _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word8 where
  toPrim _ a = PersistInt64 (fromIntegral a)
  fromPrim _ (PersistInt64 a) = fromIntegral a
  fromPrim _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word16 where
  toPrim _ a = PersistInt64 (fromIntegral a)
  fromPrim _ (PersistInt64 a) = fromIntegral a
  fromPrim _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word32 where
  toPrim _ a = PersistInt64 (fromIntegral a)
  fromPrim _ (PersistInt64 a) = fromIntegral a
  fromPrim _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word64 where
  toPrim _ a = PersistInt64 (fromIntegral a)
  fromPrim _ (PersistInt64 a) = fromIntegral a
  fromPrim _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Double where
  toPrim _ a = PersistDouble a
  fromPrim _ (PersistDouble a) = a
  fromPrim _ x = readHelper x ("Expected Double, received: " ++ show x)

instance PrimitivePersistField Bool where
  toPrim _ a = PersistBool a
  fromPrim _ (PersistBool a) = a
  fromPrim _ (PersistInt64 i) = i /= 0
  fromPrim _ x = error $ "Expected Bool, received: " ++ show x

instance PrimitivePersistField Day where
  toPrim _ a = PersistDay a
  fromPrim _ (PersistDay a) = a
  fromPrim _ x = readHelper x ("Expected Day, received: " ++ show x)

instance PrimitivePersistField TimeOfDay where
  toPrim _ a = PersistTimeOfDay a
  fromPrim _ (PersistTimeOfDay a) = a
  fromPrim _ x = readHelper x ("Expected TimeOfDay, received: " ++ show x)

instance PrimitivePersistField UTCTime where
  toPrim _ a = PersistUTCTime a
  fromPrim _ (PersistUTCTime a) = a
  fromPrim _ x = readHelper x ("Expected UTCTime, received: " ++ show x)

instance (PrimitivePersistField a, NeverNull a) => PrimitivePersistField (Maybe a) where
  toPrim p a = maybe PersistNull (toPrim p) a
  fromPrim _ PersistNull = Nothing
  fromPrim p x = Just $ fromPrim p x

instance (DbDescriptor db, PersistEntity v) => PrimitivePersistField (KeyForBackend db v) where
  toPrim p (KeyForBackend a) = toPrim p a
  fromPrim p x = KeyForBackend (fromPrim p x)

instance NeverNull String
instance NeverNull T.Text
instance NeverNull ByteString
instance NeverNull Int
instance NeverNull Int64
instance NeverNull Double
instance NeverNull Bool
instance NeverNull Day
instance NeverNull TimeOfDay
instance NeverNull UTCTime
instance NeverNull (Key v u)
instance NeverNull (KeyForBackend db v)

readHelper :: Read a => PersistValue -> String -> a
readHelper s errMessage = case s of
  PersistString str -> readHelper' str
  PersistByteString str -> readHelper' (unpack str)
  _ -> error errMessage
  where
    readHelper' str = case reads str of
      (a, _):_ -> a
      _        -> error errMessage

instance PersistField ByteString where
  persistName _ = "ByteString"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbBlob

instance PersistField String where
  persistName _ = "String"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbString

instance PersistField T.Text where
  persistName _ = "Text"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbString

instance PersistField Int where
  persistName _ = "Int"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType a = if bitSize a == 32 then DbInt32 else DbInt64

instance PersistField Int8 where
  persistName _ = "Int8"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Int16 where
  persistName _ = "Int16"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Int32 where
  persistName _ = "Int32"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Int64 where
  persistName _ = "Int64"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt64

instance PersistField Word8 where
  persistName _ = "Word8"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Word16 where
  persistName _ = "Word16"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Word32 where
  persistName _ = "Word32"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt64

instance PersistField Word64 where
  persistName _ = "Word64"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt64

instance PersistField Double where
  persistName _ = "Double"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbReal

instance PersistField Bool where
  persistName _ = "Bool"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbBool

instance PersistField Day where
  persistName _ = "Day"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbDay

instance PersistField TimeOfDay where
  persistName _ = "TimeOfDay"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTime

instance PersistField UTCTime where
  persistName _ = "UTCTime"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbDayTime

instance (SinglePersistField a, NeverNull a) => PersistField (Maybe a) where
  persistName a = "Maybe$" ++ persistName ((undefined :: Maybe a -> a) a)
  toPersistValues Nothing = return (PersistNull:)
  toPersistValues (Just a) = toSinglePersistValue a >>= \x -> return (x:)
  fromPersistValues [] = fail "fromPersistValues Maybe: empty list"
  fromPersistValues (PersistNull:xs) = return (Nothing, xs)
  fromPersistValues (x:xs) = fromSinglePersistValue x >>= \x' -> return (Just x', xs)
  dbType a = DbMaybe $ dbType ((undefined :: Maybe a -> a) a)

instance (PersistField a) => PersistField [a] where
  persistName a = "List$$" ++ persistName ((undefined :: [] a -> a) a)
  toPersistValues l = insertList l >>= toPersistValues
  fromPersistValues [] = fail "fromPersistValues []: empty list"
  fromPersistValues (x:xs) = phantomDb >>= \p -> getList (fromPrim p x) >>= \l -> return (l, xs)
  dbType a = DbList (persistName a) $ dbType ((undefined :: [] a -> a) a)

instance PersistField () where
  persistName _ = "Unit$"
  toPersistValues _ = return id
  fromPersistValues xs = return ((), xs)
  dbType _ = DbEmbedded $ EmbeddedDef False []

instance (PersistField a, PersistField b) => PersistField (a, b) where
  persistName a = "Tuple2$$" ++ persistName ((undefined :: (a, b) -> a) a) ++ "$" ++ persistName ((undefined :: (a, b) -> b) a)
  toPersistValues (a, b) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    return $ a' . b'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    return ((a, b), rest1)
  dbType a = DbEmbedded $ EmbeddedDef False [("val0", dbType ((undefined :: (a, b) -> a) a)), ("val1", dbType ((undefined :: (a, b) -> b) a))]
  
instance (PersistField a, PersistField b, PersistField c) => PersistField (a, b, c) where
  persistName a = "Tuple3$$" ++ persistName ((undefined :: (a, b, c) -> a) a) ++ "$" ++ persistName ((undefined :: (a, b, c) -> b) a) ++ "$" ++ persistName ((undefined :: (a, b, c) -> c) a)
  toPersistValues (a, b, c) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    c' <- toPersistValues c
    return $ a' . b' . c'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    (c, rest2) <- fromPersistValues rest1
    return ((a, b, c), rest2)
  dbType a = DbEmbedded $ EmbeddedDef False [("val0", dbType ((undefined :: (a, b, c) -> a) a)), ("val1", dbType ((undefined :: (a, b, c) -> b) a)), ("val2", dbType ((undefined :: (a, b, c) -> c) a))]
  
instance (PersistField a, PersistField b, PersistField c, PersistField d) => PersistField (a, b, c, d) where
  persistName a = "Tuple4$$" ++ persistName ((undefined :: (a, b, c, d) -> a) a) ++ "$" ++ persistName ((undefined :: (a, b, c, d) -> b) a) ++ "$" ++ persistName ((undefined :: (a, b, c, d) -> c) a) ++ "$" ++ persistName ((undefined :: (a, b, c, d) -> d) a)
  toPersistValues (a, b, c, d) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    c' <- toPersistValues c
    d' <- toPersistValues d
    return $ a' . b' . c' . d'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    (c, rest2) <- fromPersistValues rest1
    (d, rest3) <- fromPersistValues rest2
    return ((a, b, c, d), rest3)
  dbType a = DbEmbedded $ EmbeddedDef False [("val0", dbType ((undefined :: (a, b, c, d) -> a) a)), ("val1", dbType ((undefined :: (a, b, c, d) -> b) a)), ("val2", dbType ((undefined :: (a, b, c, d) -> c) a)), ("val3", dbType ((undefined :: (a, b, c, d) -> d) a))]
  
instance (PersistField a, PersistField b, PersistField c, PersistField d, PersistField e) => PersistField (a, b, c, d, e) where
  persistName a = "Tuple5$$" ++ persistName ((undefined :: (a, b, c, d, e) -> a) a) ++ "$" ++ persistName ((undefined :: (a, b, c, d, e) -> b) a) ++ "$" ++ persistName ((undefined :: (a, b, c, d, e) -> c) a) ++ "$" ++ persistName ((undefined :: (a, b, c, d, e) -> d) a) ++ "$" ++ persistName ((undefined :: (a, b, c, d, e) -> e) a)
  toPersistValues (a, b, c, d, e) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    c' <- toPersistValues c
    d' <- toPersistValues d
    e' <- toPersistValues e
    return $ a' . b' . c' . d' . e'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    (c, rest2) <- fromPersistValues rest1
    (d, rest3) <- fromPersistValues rest2
    (e, rest4) <- fromPersistValues rest3
    return ((a, b, c, d, e), rest4)
  dbType a = DbEmbedded $ EmbeddedDef False [("val0", dbType ((undefined :: (a, b, c, d, e) -> a) a)), ("val1", dbType ((undefined :: (a, b, c, d, e) -> b) a)), ("val2", dbType ((undefined :: (a, b, c, d, e) -> c) a)), ("val3", dbType ((undefined :: (a, b, c, d, e) -> d) a)), ("val4", dbType ((undefined :: (a, b, c, d, e) -> e) a))]

instance (DbDescriptor db, PersistEntity v) => PersistField (KeyForBackend db v) where
  persistName a = "KeyForBackend$" ++ persistName ((undefined :: KeyForBackend db v -> v) a)
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType a = dbType ((undefined :: KeyForBackend db v -> v) a)

instance (PersistEntity v, Constructor c, PersistField a) => Projection (Field v c a) (RestrictionHolder v c) a where
  projectionFieldChains f = (fieldChain f:)
  projectionResult _ = fromPersistValues

instance (PersistEntity v, Constructor c, PersistField a) => Projection (SubField v c a) (RestrictionHolder v c) a where
  projectionFieldChains f = (fieldChain f:)
  projectionResult _ = fromPersistValues

instance (PersistEntity v, Constructor c, PersistField (Key v BackendSpecific)) => Projection (AutoKeyField v c) (RestrictionHolder v c) (Key v BackendSpecific) where
  projectionFieldChains f = (fieldChain f:)
  projectionResult _ = fromPersistValues

instance (PersistEntity v, Constructor c) => Projection (c (ConstructorMarker v)) (RestrictionHolder v c) v where
  projectionFieldChains c = (chains++) where
    chains = map (\f -> (f, [])) $ constrParams constr
    e = entityDef ((undefined :: c (ConstructorMarker v) -> v) c)
    constr = constructors e !! phantomConstrNum c
  projectionResult c xs = toSinglePersistValue (phantomConstrNum c) >>= \cNum -> fromEntityPersistValues (cNum:xs)

instance (PersistEntity v, IsUniqueKey (Key v (Unique u)), r ~ RestrictionHolder v (UniqueConstr (Key v (Unique u))))
      => Projection (u (UniqueMarker v)) r (Key v (Unique u)) where
  projectionFieldChains u = (chains++) where
    UniqueDef _ uFields = constrUniques constr !! uniqueNum ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u)
    chains = map (\f -> (f, [])) uFields
    constr = head $ constructors (entityDef ((undefined :: u (UniqueMarker v) -> v) u))
  projectionResult _ = fromPersistValues

instance (Projection a1 r a1', Projection a2 r a2') => Projection (a1, a2) r (a1', a2') where
  projectionFieldChains (a1, a2) = projectionFieldChains a1 . projectionFieldChains a2
  projectionResult (a', b') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    return ((a, b), rest1)

instance (Projection a1 r a1', Projection a2 r a2', Projection a3 r a3') => Projection (a1, a2, a3) r (a1', a2', a3') where
  projectionFieldChains (a1, a2, a3) = projectionFieldChains a1 . projectionFieldChains a2 . projectionFieldChains a3
  projectionResult (a', b', c') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    return ((a, b, c), rest2)

instance (Projection a1 r a1', Projection a2 r a2', Projection a3 r a3', Projection a4 r a4') => Projection (a1, a2, a3, a4) r (a1', a2', a3', a4') where
  projectionFieldChains (a1, a2, a3, a4) = projectionFieldChains a1 . projectionFieldChains a2 . projectionFieldChains a3 . projectionFieldChains a4
  projectionResult (a', b', c', d') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    return ((a, b, c, d), rest3)

instance (Projection a1 r a1', Projection a2 r a2', Projection a3 r a3', Projection a4 r a4', Projection a5 r a5') => Projection (a1, a2, a3, a4, a5) r (a1', a2', a3', a4', a5') where
  projectionFieldChains (a1, a2, a3, a4, a5) = projectionFieldChains a1 . projectionFieldChains a2 . projectionFieldChains a3 . projectionFieldChains a4 . projectionFieldChains a5
  projectionResult (a', b', c', d', e') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    return ((a, b, c, d, e), rest4)

instance (PersistEntity v, Constructor c, Projection (AutoKeyField v c) r a') => FieldLike (AutoKeyField v c) r a' where
  fieldChain a = chain where
    chain = maybe (error "fieldChain AutoKeyField: constructor constrAutoKeyName == Nothing") (\idName -> ((idName, DbEntity Nothing e), [])) $ constrAutoKeyName constr
    e = entityDef ((undefined :: AutoKeyField v c -> v) a)
    cNum = phantomConstrNum ((undefined :: AutoKeyField v c -> c (ConstructorMarker v)) a)
    constr = constructors e !! cNum

instance (PersistEntity v, Constructor c, Projection (SubField v c a) r a') => FieldLike (SubField v c a) r a' where
  fieldChain (SubField a) = a

instance (PersistEntity v, Constructor c, Projection (Field v c a) r a') => FieldLike (Field v c a) r a' where
  fieldChain = entityFieldChain

instance (PersistEntity v, IsUniqueKey (Key v (Unique u)), Projection (u (UniqueMarker v)) r a') => FieldLike (u (UniqueMarker v)) r a' where
  fieldChain u = chain where
    UniqueDef _ uFields = constrUniques constr !! uniqueNum ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u)
    chain = (("will_be_ignored", DbEmbedded $ EmbeddedDef True $ uFields), [])
    constr = head $ constructors (entityDef ((undefined :: u (UniqueMarker v) -> v) u))
