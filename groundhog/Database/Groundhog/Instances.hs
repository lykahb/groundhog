{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Groundhog.Instances (Selector (..)) where

import qualified Data.Aeson as A
import Data.Bits (finiteBitSize)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe)
import qualified Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as TL
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Time.LocalTime (ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Groundhog.Core
import Database.Groundhog.Generic (getUniqueFields, primFromPersistValue, primFromPurePersistValues, primFromSinglePersistValue, primToPersistValue, primToPurePersistValues, primToSinglePersistValue)

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

instance PurePersistField () where
  toPurePersistValues _ = id
  fromPurePersistValues xs = ((), xs)

instance (PurePersistField a, PurePersistField b) => PurePersistField (a, b) where
  toPurePersistValues (a, b) = toPurePersistValues a . toPurePersistValues b
  fromPurePersistValues xs =
    let (a, rest0) = fromPurePersistValues xs
        (b, rest1) = fromPurePersistValues rest0
     in ((a, b), rest1)

instance (PurePersistField a, PurePersistField b, PurePersistField c) => PurePersistField (a, b, c) where
  toPurePersistValues (a, b, c) = toPurePersistValues a . toPurePersistValues b . toPurePersistValues c
  fromPurePersistValues xs =
    let (a, rest0) = fromPurePersistValues xs
        (b, rest1) = fromPurePersistValues rest0
        (c, rest2) = fromPurePersistValues rest1
     in ((a, b, c), rest2)

instance (PurePersistField a, PurePersistField b, PurePersistField c, PurePersistField d) => PurePersistField (a, b, c, d) where
  toPurePersistValues (a, b, c, d) = toPurePersistValues a . toPurePersistValues b . toPurePersistValues c . toPurePersistValues d
  fromPurePersistValues xs =
    let (a, rest0) = fromPurePersistValues xs
        (b, rest1) = fromPurePersistValues rest0
        (c, rest2) = fromPurePersistValues rest1
        (d, rest3) = fromPurePersistValues rest2
     in ((a, b, c, d), rest3)

instance (PurePersistField a, PurePersistField b, PurePersistField c, PurePersistField d, PurePersistField e) => PurePersistField (a, b, c, d, e) where
  toPurePersistValues (a, b, c, d, e) = toPurePersistValues a . toPurePersistValues b . toPurePersistValues c . toPurePersistValues d . toPurePersistValues e
  fromPurePersistValues xs =
    let (a, rest0) = fromPurePersistValues xs
        (b, rest1) = fromPurePersistValues rest0
        (c, rest2) = fromPurePersistValues rest1
        (d, rest3) = fromPurePersistValues rest2
        (e, rest4) = fromPurePersistValues rest3
     in ((a, b, c, d, e), rest4)

instance PrimitivePersistField String where
  toPrimitivePersistValue s = PersistText (T.pack s)
  fromPrimitivePersistValue (PersistString s) = s
  fromPrimitivePersistValue (PersistText s) = T.unpack s
  fromPrimitivePersistValue (PersistByteString bs) = T.unpack $ T.decodeUtf8With T.lenientDecode bs
  fromPrimitivePersistValue (PersistInt64 i) = show i
  fromPrimitivePersistValue (PersistDouble d) = show d
  fromPrimitivePersistValue (PersistDay d) = show d
  fromPrimitivePersistValue (PersistTimeOfDay d) = show d
  fromPrimitivePersistValue (PersistUTCTime d) = show d
  fromPrimitivePersistValue (PersistZonedTime z) = show z
  fromPrimitivePersistValue (PersistBool b) = show b
  fromPrimitivePersistValue PersistNull = error "Unexpected NULL"
  fromPrimitivePersistValue (PersistCustom _ _) = error "Unexpected PersistCustom"

instance PrimitivePersistField T.Text where
  toPrimitivePersistValue s = PersistText s
  fromPrimitivePersistValue (PersistText s) = s
  fromPrimitivePersistValue (PersistByteString bs) = T.decodeUtf8With T.lenientDecode bs
  fromPrimitivePersistValue x = T.pack $ fromPrimitivePersistValue x

instance PrimitivePersistField TL.Text where
  toPrimitivePersistValue s = toPrimitivePersistValue (TL.toStrict s)
  fromPrimitivePersistValue x = TL.fromStrict $ fromPrimitivePersistValue x

instance PrimitivePersistField ByteString where
  toPrimitivePersistValue s = PersistByteString s
  fromPrimitivePersistValue (PersistByteString a) = a
  fromPrimitivePersistValue x = T.encodeUtf8 . T.pack $ fromPrimitivePersistValue x

instance PrimitivePersistField Lazy.ByteString where
  toPrimitivePersistValue s = PersistByteString $ Lazy.toStrict s
  fromPrimitivePersistValue (PersistByteString a) = Lazy.fromStrict a
  fromPrimitivePersistValue x = Lazy.fromStrict . T.encodeUtf8 . T.pack $ fromPrimitivePersistValue x

instance PrimitivePersistField Int where
  toPrimitivePersistValue a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue (PersistDouble a) = truncate a
  fromPrimitivePersistValue x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int8 where
  toPrimitivePersistValue a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue (PersistDouble a) = truncate a
  fromPrimitivePersistValue x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int16 where
  toPrimitivePersistValue a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue (PersistDouble a) = truncate a
  fromPrimitivePersistValue x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int32 where
  toPrimitivePersistValue a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue (PersistDouble a) = truncate a
  fromPrimitivePersistValue x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int64 where
  toPrimitivePersistValue a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue (PersistInt64 a) = a
  fromPrimitivePersistValue (PersistDouble a) = truncate a
  fromPrimitivePersistValue x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word8 where
  toPrimitivePersistValue a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue (PersistDouble a) = truncate a
  fromPrimitivePersistValue x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word16 where
  toPrimitivePersistValue a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue (PersistDouble a) = truncate a
  fromPrimitivePersistValue x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word32 where
  toPrimitivePersistValue a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue (PersistDouble a) = truncate a
  fromPrimitivePersistValue x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word64 where
  toPrimitivePersistValue a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue (PersistDouble a) = truncate a
  fromPrimitivePersistValue x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Double where
  toPrimitivePersistValue a = PersistDouble a
  fromPrimitivePersistValue (PersistDouble a) = a
  fromPrimitivePersistValue (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue x = readHelper x ("Expected Double, received: " ++ show x)

instance PrimitivePersistField Bool where
  toPrimitivePersistValue a = PersistBool a
  fromPrimitivePersistValue (PersistBool a) = a
  fromPrimitivePersistValue (PersistInt64 i) = i /= 0
  fromPrimitivePersistValue x = error $ "Expected Bool, received: " ++ show x

instance PrimitivePersistField Day where
  toPrimitivePersistValue a = PersistDay a
  fromPrimitivePersistValue (PersistDay a) = a
  fromPrimitivePersistValue x = readHelper x ("Expected Day, received: " ++ show x)

instance PrimitivePersistField TimeOfDay where
  toPrimitivePersistValue a = PersistTimeOfDay a
  fromPrimitivePersistValue (PersistTimeOfDay a) = a
  fromPrimitivePersistValue x = readHelper x ("Expected TimeOfDay, received: " ++ show x)

instance PrimitivePersistField UTCTime where
  toPrimitivePersistValue a = PersistUTCTime a
  fromPrimitivePersistValue (PersistUTCTime a) = a
  fromPrimitivePersistValue (PersistZonedTime (ZT a)) = zonedTimeToUTC a
  fromPrimitivePersistValue x = readHelper x ("Expected UTCTime, received: " ++ show x)

instance PrimitivePersistField ZonedTime where
  toPrimitivePersistValue a = PersistZonedTime (ZT a)
  fromPrimitivePersistValue (PersistZonedTime (ZT a)) = a
  fromPrimitivePersistValue (PersistUTCTime a) = utcToZonedTime utc a
  fromPrimitivePersistValue x = readHelper x ("Expected ZonedTime, received: " ++ show x)

instance PrimitivePersistField A.Value where
  toPrimitivePersistValue a = PersistByteString $ Lazy.toStrict $ A.encode a
  fromPrimitivePersistValue x =
    case x of
      PersistString str -> decode' $ T.encodeUtf8 $ T.pack str
      PersistText str -> decode' $ T.encodeUtf8 str
      PersistByteString str -> decode' str
      _ -> error $ "Expected Aeson.Value, received: " ++ show x
    where
      decode' str = case A.eitherDecode $ Lazy.fromStrict str of
        Right val -> val
        Left err -> error $ "Error decoding Aeson.Value: " ++ err

instance (PrimitivePersistField a, NeverNull a) => PrimitivePersistField (Maybe a) where
  toPrimitivePersistValue a = maybe PersistNull toPrimitivePersistValue a
  fromPrimitivePersistValue PersistNull = Nothing
  fromPrimitivePersistValue x = Just $ fromPrimitivePersistValue x

instance (DbDescriptor db, PersistEntity v, PersistField v) => PrimitivePersistField (KeyForBackend db v) where
  toPrimitivePersistValue (KeyForBackend a) = toPrimitivePersistValue a
  fromPrimitivePersistValue x = KeyForBackend (fromPrimitivePersistValue x)

instance {-# OVERLAPPABLE #-} (PersistField a, PrimitivePersistField a) => PurePersistField a where
  toPurePersistValues = primToPurePersistValues
  fromPurePersistValues = primFromPurePersistValues

instance {-# OVERLAPPABLE #-} (PersistField a, PrimitivePersistField a) => SinglePersistField a where
  toSinglePersistValue = primToSinglePersistValue
  fromSinglePersistValue = primFromSinglePersistValue

instance NeverNull String

instance NeverNull T.Text

instance NeverNull TL.Text

instance NeverNull ByteString

instance NeverNull Lazy.ByteString

instance NeverNull Int

instance NeverNull Int8

instance NeverNull Int16

instance NeverNull Int32

instance NeverNull Int64

instance NeverNull Word8

instance NeverNull Word16

instance NeverNull Word32

instance NeverNull Word64

instance NeverNull Double

instance NeverNull Bool

instance NeverNull Day

instance NeverNull TimeOfDay

instance NeverNull UTCTime

instance NeverNull ZonedTime

instance NeverNull A.Value

instance PrimitivePersistField (Key v u) => NeverNull (Key v u)

instance NeverNull (KeyForBackend db v)

readHelper :: Read a => PersistValue -> String -> a
readHelper s errMessage = case s of
  PersistString str -> readHelper' str
  PersistText str -> readHelper' (T.unpack str)
  PersistByteString str -> readHelper' (unpack str)
  _ -> error $ "readHelper: " ++ errMessage
  where
    readHelper' str = case reads str of
      (a, _) : _ -> a
      _ -> error $ "readHelper: " ++ errMessage

instance PersistField ByteString where
  persistName _ = "ByteString"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbBlob False Nothing Nothing

instance PersistField Lazy.ByteString where
  persistName _ = "ByteString"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbBlob False Nothing Nothing

instance PersistField A.Value where
  persistName _ = "JsonValue"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbBlob False Nothing Nothing

instance PersistField String where
  persistName _ = "String"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PersistField T.Text where
  persistName _ = "Text"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PersistField TL.Text where
  persistName _ = "Text"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PersistField Int where
  persistName _ = "Int"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ a = DbTypePrimitive (if finiteBitSize a == 32 then DbInt32 else DbInt64) False Nothing Nothing where

#if !MIN_VERSION_base(4, 7, 0)
    finiteBitSize = bitSize
#endif

instance PersistField Int8 where
  persistName _ = "Int8"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt32 False Nothing Nothing

instance PersistField Int16 where
  persistName _ = "Int16"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt32 False Nothing Nothing

instance PersistField Int32 where
  persistName _ = "Int32"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt32 False Nothing Nothing

instance PersistField Int64 where
  persistName _ = "Int64"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt64 False Nothing Nothing

instance PersistField Word8 where
  persistName _ = "Word8"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt32 False Nothing Nothing

instance PersistField Word16 where
  persistName _ = "Word16"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt32 False Nothing Nothing

instance PersistField Word32 where
  persistName _ = "Word32"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt64 False Nothing Nothing

instance PersistField Word64 where
  persistName _ = "Word64"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt64 False Nothing Nothing

instance PersistField Double where
  persistName _ = "Double"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbReal False Nothing Nothing

instance PersistField Bool where
  persistName _ = "Bool"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbBool False Nothing Nothing

instance PersistField Day where
  persistName _ = "Day"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbDay False Nothing Nothing

instance PersistField TimeOfDay where
  persistName _ = "TimeOfDay"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbTime False Nothing Nothing

instance PersistField UTCTime where
  persistName _ = "UTCTime"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbDayTime False Nothing Nothing

instance PersistField ZonedTime where
  persistName _ = "ZonedTime"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbDayTimeZoned False Nothing Nothing

-- There is a weird bug in GHC 7.4.1 which causes program to hang. See ticket 7126.
-- instance (PersistField a, NeverNull a) => PersistField (Maybe a) where -- OK
-- instance (SinglePersistField a, NeverNull a) => PersistField (Maybe a) where -- HANGS
instance (PersistField a, NeverNull a) => PersistField (Maybe a) where
  persistName a = "Maybe" ++ delim : persistName ((undefined :: Maybe a -> a) a)
  toPersistValues Nothing = pure (PersistNull :)
  toPersistValues (Just a) = toPersistValues a
  fromPersistValues [] = fail "fromPersistValues Maybe: empty list"
  fromPersistValues (PersistNull : xs) = pure (Nothing, xs)
  fromPersistValues xs = fromPersistValues xs >>= \(x, xs') -> pure (Just x, xs')
  dbType db a = case dbType db ((undefined :: Maybe a -> a) a) of
    DbTypePrimitive t _ def ref -> DbTypePrimitive t True def ref
    DbEmbedded (EmbeddedDef concatName [(field, DbTypePrimitive t _ def ref')]) ref ->
      DbEmbedded (EmbeddedDef concatName [(field, DbTypePrimitive t True def ref')]) ref
    t -> error $ "dbType " ++ persistName a ++ ": expected DbTypePrimitive or DbEmbedded with one field, got " ++ show t

instance {-# OVERLAPPABLE #-} (PersistField a) => PersistField [a] where
  persistName a = "List" ++ delim : delim : persistName ((undefined :: [] a -> a) a)
  toPersistValues l = insertList l >>= toPersistValues
  fromPersistValues [] = fail "fromPersistValues []: empty list"
  fromPersistValues (x : xs) = getList (fromPrimitivePersistValue x) >>= \l -> pure (l, xs)
  dbType db a = DbList (persistName a) $ dbType db ((undefined :: [] a -> a) a)

instance PersistField () where
  persistName _ = "Unit" ++ [delim]
  toPersistValues _ = pure id
  fromPersistValues xs = pure ((), xs)
  dbType _ _ = DbEmbedded (EmbeddedDef False []) Nothing

instance (PersistField a, PersistField b) => PersistField (a, b) where
  persistName a = "Tuple2" ++ delim : delim : persistName ((undefined :: (a, b) -> a) a) ++ delim : persistName ((undefined :: (a, b) -> b) a)
  toPersistValues (a, b) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    pure $ a' . b'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    pure ((a, b), rest1)
  dbType db a = DbEmbedded (EmbeddedDef False [("val0", dbType db ((undefined :: (a, b) -> a) a)), ("val1", dbType db ((undefined :: (a, b) -> b) a))]) Nothing

instance (PersistField a, PersistField b, PersistField c) => PersistField (a, b, c) where
  persistName a = "Tuple3" ++ delim : delim : persistName ((undefined :: (a, b, c) -> a) a) ++ delim : persistName ((undefined :: (a, b, c) -> b) a) ++ delim : persistName ((undefined :: (a, b, c) -> c) a)
  toPersistValues (a, b, c) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    c' <- toPersistValues c
    pure $ a' . b' . c'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    (c, rest2) <- fromPersistValues rest1
    pure ((a, b, c), rest2)
  dbType db a = DbEmbedded (EmbeddedDef False [("val0", dbType db ((undefined :: (a, b, c) -> a) a)), ("val1", dbType db ((undefined :: (a, b, c) -> b) a)), ("val2", dbType db ((undefined :: (a, b, c) -> c) a))]) Nothing

instance (PersistField a, PersistField b, PersistField c, PersistField d) => PersistField (a, b, c, d) where
  persistName a = "Tuple4" ++ delim : delim : persistName ((undefined :: (a, b, c, d) -> a) a) ++ delim : persistName ((undefined :: (a, b, c, d) -> b) a) ++ delim : persistName ((undefined :: (a, b, c, d) -> c) a) ++ delim : persistName ((undefined :: (a, b, c, d) -> d) a)
  toPersistValues (a, b, c, d) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    c' <- toPersistValues c
    d' <- toPersistValues d
    pure $ a' . b' . c' . d'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    (c, rest2) <- fromPersistValues rest1
    (d, rest3) <- fromPersistValues rest2
    pure ((a, b, c, d), rest3)
  dbType db a = DbEmbedded (EmbeddedDef False [("val0", dbType db ((undefined :: (a, b, c, d) -> a) a)), ("val1", dbType db ((undefined :: (a, b, c, d) -> b) a)), ("val2", dbType db ((undefined :: (a, b, c, d) -> c) a)), ("val3", dbType db ((undefined :: (a, b, c, d) -> d) a))]) Nothing

instance (PersistField a, PersistField b, PersistField c, PersistField d, PersistField e) => PersistField (a, b, c, d, e) where
  persistName a = "Tuple5" ++ delim : delim : persistName ((undefined :: (a, b, c, d, e) -> a) a) ++ delim : persistName ((undefined :: (a, b, c, d, e) -> b) a) ++ delim : persistName ((undefined :: (a, b, c, d, e) -> c) a) ++ delim : persistName ((undefined :: (a, b, c, d, e) -> d) a) ++ delim : persistName ((undefined :: (a, b, c, d, e) -> e) a)
  toPersistValues (a, b, c, d, e) = do
    a' <- toPersistValues a
    b' <- toPersistValues b
    c' <- toPersistValues c
    d' <- toPersistValues d
    e' <- toPersistValues e
    pure $ a' . b' . c' . d' . e'
  fromPersistValues xs = do
    (a, rest0) <- fromPersistValues xs
    (b, rest1) <- fromPersistValues rest0
    (c, rest2) <- fromPersistValues rest1
    (d, rest3) <- fromPersistValues rest2
    (e, rest4) <- fromPersistValues rest3
    pure ((a, b, c, d, e), rest4)
  dbType db a = DbEmbedded (EmbeddedDef False [("val0", dbType db ((undefined :: (a, b, c, d, e) -> a) a)), ("val1", dbType db ((undefined :: (a, b, c, d, e) -> b) a)), ("val2", dbType db ((undefined :: (a, b, c, d, e) -> c) a)), ("val3", dbType db ((undefined :: (a, b, c, d, e) -> d) a)), ("val4", dbType db ((undefined :: (a, b, c, d, e) -> e) a))]) Nothing

instance (DbDescriptor db, PersistEntity v, PersistField v) => PersistField (KeyForBackend db v) where
  persistName a = "KeyForBackend" ++ delim : persistName ((undefined :: KeyForBackend db v -> v) a)
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType db a = dbType db ((undefined :: KeyForBackend db v -> DefaultKey v) a)

instance (EntityConstr v c, PersistField a) => Projection (Field v c a) a where
  type ProjectionDb (Field v c a) db = ()
  type ProjectionRestriction (Field v c a) r = r ~ RestrictionHolder v c
  projectionExprs f = result
    where
      result = (ExprField (fieldChain db f) :)
      db = (undefined :: ([UntypedExpr db r] -> [UntypedExpr db r]) -> proxy db) result
  projectionResult _ = fromPersistValues

instance (EntityConstr v c, PersistField a) => Projection (SubField db v c a) a where
  type ProjectionDb (SubField db v c a) db' = db ~ db'
  type ProjectionRestriction (SubField db v c a) r = r ~ RestrictionHolder v c
  projectionExprs f = result
    where
      result = (ExprField (fieldChain db f) :)
      db = (undefined :: ([UntypedExpr db r] -> [UntypedExpr db r]) -> proxy db) result
  projectionResult _ = fromPersistValues

instance PersistField a => Projection (Expr db r a) a where
  type ProjectionDb (Expr db r a) db' = db ~ db'
  type ProjectionRestriction (Expr db r a) r' = r ~ r'
  projectionExprs (Expr e) = (e :)
  projectionResult _ = fromPersistValues

instance a ~ Bool => Projection (Cond db r) a where
  type ProjectionDb (Cond db r) db' = db ~ db'
  type ProjectionRestriction (Cond db r) r' = r ~ r'
  projectionExprs cond = (ExprCond cond :)
  projectionResult _ = fromPersistValues

instance (EntityConstr v c, a ~ AutoKey v) => Projection (AutoKeyField v c) a where
  type ProjectionDb (AutoKeyField v c) db = ()
  type ProjectionRestriction (AutoKeyField v c) r = r ~ RestrictionHolder v c
  projectionExprs f = result
    where
      result = (ExprField (fieldChain db f) :)
      db = (undefined :: ([UntypedExpr db r] -> [UntypedExpr db r]) -> proxy db) result
  projectionResult _ = fromPersistValues

instance EntityConstr v c => Projection (c (ConstructorMarker v)) v where
  type ProjectionDb (c (ConstructorMarker v)) db = ()
  type ProjectionRestriction (c (ConstructorMarker v)) r = r ~ RestrictionHolder v c
  projectionExprs c = result
    where
      result = (map ExprField chains ++)
      chains = map (\f -> (f, [])) $ constrParams constr
      e = entityDef db ((undefined :: c (ConstructorMarker v) -> v) c)
      cNum = entityConstrNum ((undefined :: c (ConstructorMarker v) -> proxy v) c) c
      constr = constructors e !! cNum
      db = (undefined :: ([UntypedExpr db r] -> [UntypedExpr db r]) -> proxy db) result
  projectionResult c xs = toSinglePersistValue cNum >>= \cNum' -> fromEntityPersistValues (cNum' : xs)
    where
      cNum = entityConstrNum ((undefined :: c (ConstructorMarker v) -> proxy v) c) c

instance
  (PersistEntity v, IsUniqueKey k, k ~ Key v (Unique u)) =>
  Projection (u (UniqueMarker v)) k
  where
  type ProjectionDb (u (UniqueMarker v)) db = ()
  type ProjectionRestriction (u (UniqueMarker v)) (RestrictionHolder v' c) = v ~ v'
  projectionExprs u = result
    where
      result = (map ExprField chains ++)
      uDef = constrUniques constr !! uniqueNum ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u)
      chains = map (\f -> (f, [])) $ getUniqueFields uDef
      constr = head $ constructors (entityDef db ((undefined :: u (UniqueMarker v) -> v) u))
      db = (undefined :: ([UntypedExpr db r] -> [UntypedExpr db r]) -> proxy db) result
  projectionResult _ = fromPersistValues

instance (Projection a1 a1', Projection a2 a2') => Projection (a1, a2) (a1', a2') where
  type ProjectionDb (a1, a2) db = (ProjectionDb a1 db, ProjectionDb a2 db)
  type ProjectionRestriction (a1, a2) r = (ProjectionRestriction a1 r, ProjectionRestriction a2 r)
  projectionExprs (a1, a2) = projectionExprs a1 . projectionExprs a2
  projectionResult (a', b') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    pure ((a, b), rest1)

instance (Projection a1 a1', Projection a2 a2', Projection a3 a3') => Projection (a1, a2, a3) (a1', a2', a3') where
  type ProjectionDb (a1, a2, a3) db = (ProjectionDb (a1, a2) db, ProjectionDb a3 db)
  type ProjectionRestriction (a1, a2, a3) r = (ProjectionRestriction (a1, a2) r, ProjectionRestriction a3 r)
  projectionExprs (a1, a2, a3) = projectionExprs a1 . projectionExprs a2 . projectionExprs a3
  projectionResult (a', b', c') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    pure ((a, b, c), rest2)

instance (Projection a1 a1', Projection a2 a2', Projection a3 a3', Projection a4 a4') => Projection (a1, a2, a3, a4) (a1', a2', a3', a4') where
  type ProjectionDb (a1, a2, a3, a4) db = (ProjectionDb (a1, a2, a3) db, ProjectionDb a4 db)
  type ProjectionRestriction (a1, a2, a3, a4) r = (ProjectionRestriction (a1, a2, a3) r, ProjectionRestriction a4 r)
  projectionExprs (a1, a2, a3, a4) = projectionExprs a1 . projectionExprs a2 . projectionExprs a3 . projectionExprs a4
  projectionResult (a', b', c', d') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    pure ((a, b, c, d), rest3)

instance (Projection a1 a1', Projection a2 a2', Projection a3 a3', Projection a4 a4', Projection a5 a5') => Projection (a1, a2, a3, a4, a5) (a1', a2', a3', a4', a5') where
  type ProjectionDb (a1, a2, a3, a4, a5) db = (ProjectionDb (a1, a2, a3, a4) db, ProjectionDb a5 db)
  type ProjectionRestriction (a1, a2, a3, a4, a5) r = (ProjectionRestriction (a1, a2, a3, a4) r, ProjectionRestriction a5 r)
  projectionExprs (a1, a2, a3, a4, a5) = projectionExprs a1 . projectionExprs a2 . projectionExprs a3 . projectionExprs a4 . projectionExprs a5
  projectionResult (a', b', c', d', e') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    pure ((a, b, c, d, e), rest4)

instance (EntityConstr v c, a ~ AutoKey v) => Assignable (AutoKeyField v c) a

instance (EntityConstr v c, PersistField a) => Assignable (SubField db v c a) a

instance (EntityConstr v c, PersistField a) => Assignable (Field v c a) a

instance (PersistEntity v, IsUniqueKey k, k ~ Key v (Unique u)) => Assignable (u (UniqueMarker v)) k

instance (EntityConstr v c, a ~ AutoKey v) => FieldLike (AutoKeyField v c) a where
  fieldChain db a = chain
    where
      chain = ((name, dbType db k), [])
      -- if it is Nothing, the name would not be used because the type will be () with no columns
      name = fromMaybe "will_be_ignored" $ constrAutoKeyName $ constructors e !! cNum
      k = (undefined :: AutoKeyField v c -> AutoKey v) a

      e = entityDef db ((undefined :: AutoKeyField v c -> v) a)
      cNum = entityConstrNum ((undefined :: AutoKeyField v c -> proxy v) a) ((undefined :: AutoKeyField v c -> c (ConstructorMarker v)) a)

instance (EntityConstr v c, PersistField a) => FieldLike (SubField db v c a) a where
  fieldChain _ (SubField a) = a

instance (EntityConstr v c, PersistField a) => FieldLike (Field v c a) a where
  fieldChain = entityFieldChain

instance
  (PersistEntity v, IsUniqueKey k, k ~ Key v (Unique u)) =>
  FieldLike (u (UniqueMarker v)) k
  where
  fieldChain db u = chain
    where
      uDef = constrUniques constr !! uniqueNum ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u)
      chain = (("will_be_ignored", DbEmbedded (EmbeddedDef True $ getUniqueFields uDef) Nothing), [])
      constr = head $ constructors (entityDef db ((undefined :: u (UniqueMarker v) -> v) u))

instance (PersistEntity v, EntityConstr' (IsSumType v) c) => EntityConstr v c where
  entityConstrNum v = entityConstrNum' $ (undefined :: proxy v -> IsSumType v) v

class EntityConstr' flag c where
  entityConstrNum' :: flag -> c (a :: * -> *) -> Int

instance EntityConstr' HFalse c where
  entityConstrNum' _ _ = 0

instance Constructor c => EntityConstr' HTrue c where
  entityConstrNum' _ = phantomConstrNum

instance A.FromJSON PersistValue where
  parseJSON (A.String t) = pure $ PersistText t
  parseJSON (A.Number n) =
    pure $
      if fromInteger (floor n) == n
        then PersistInt64 $ floor n
        else PersistDouble $ fromRational $ toRational n
  parseJSON (A.Bool b) = pure $ PersistBool b
  parseJSON A.Null = pure PersistNull
  parseJSON a = fail $ "parseJSON PersistValue: unexpected " ++ show a

instance A.ToJSON PersistValue where
  toJSON (PersistString t) = A.String $ T.pack t
  toJSON (PersistText t) = A.String t
  toJSON (PersistByteString b) = A.String $ T.decodeUtf8 $ B64.encode b
  toJSON (PersistInt64 i) = A.Number $ fromIntegral i
  toJSON (PersistDouble d) =
    A.Number $
      Data.Scientific.fromFloatDigits d
  toJSON (PersistBool b) = A.Bool b
  toJSON (PersistTimeOfDay t) = A.String $ T.pack $ show t
  toJSON (PersistUTCTime u) = A.String $ T.pack $ show u
  toJSON (PersistDay d) = A.String $ T.pack $ show d
  toJSON (PersistZonedTime (ZT z)) = A.String $ T.pack $ show z
  toJSON PersistNull = A.Null
  toJSON a@(PersistCustom _ _) = error $ "toJSON: unexpected " ++ show a

instance Read (Key v u) => A.FromJSON (Key v u) where
  parseJSON a = read <$> A.parseJSON a

instance Show (Key v u) => A.ToJSON (Key v u) where
  toJSON k = A.toJSON $ show k
