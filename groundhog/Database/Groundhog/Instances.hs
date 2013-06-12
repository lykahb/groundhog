{-# LANGUAGE TypeFamilies, GADTs, TypeSynonymInstances, OverlappingInstances, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Groundhog.Instances (Selector(..)) where

import Database.Groundhog.Core
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue, primToPurePersistValues, primFromPurePersistValues, primToSinglePersistValue, primFromSinglePersistValue, pureFromPersistValue, phantomDb)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Bits (bitSize)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Time.LocalTime (ZonedTime)
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

instance PrimitivePersistField String where
  toPrimitivePersistValue _ s = PersistString s
  fromPrimitivePersistValue _ (PersistString s) = s
  fromPrimitivePersistValue _ (PersistByteString bs) = T.unpack $ T.decodeUtf8With T.lenientDecode bs
  fromPrimitivePersistValue _ (PersistInt64 i) = show i
  fromPrimitivePersistValue _ (PersistDouble d) = show d
  fromPrimitivePersistValue _ (PersistDay d) = show d
  fromPrimitivePersistValue _ (PersistTimeOfDay d) = show d
  fromPrimitivePersistValue _ (PersistUTCTime d) = show d
  fromPrimitivePersistValue _ (PersistZonedTime z) = show z
  fromPrimitivePersistValue _ (PersistBool b) = show b
  fromPrimitivePersistValue _ PersistNull = error "Unexpected NULL"
  fromPrimitivePersistValue _ (PersistCustom _ _) = error "Unexpected PersistCustom"

instance PrimitivePersistField T.Text where
  toPrimitivePersistValue _ a = PersistString (T.unpack a)
  fromPrimitivePersistValue _ (PersistByteString bs) = T.decodeUtf8With T.lenientDecode bs
  fromPrimitivePersistValue p x = T.pack $ fromPrimitivePersistValue p x

instance PrimitivePersistField ByteString where
  toPrimitivePersistValue _ s = PersistByteString s
  fromPrimitivePersistValue _ (PersistByteString a) = a
  fromPrimitivePersistValue p x = T.encodeUtf8 . T.pack $ fromPrimitivePersistValue p x

instance PrimitivePersistField Int where
  toPrimitivePersistValue _ a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue _ (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int8 where
  toPrimitivePersistValue _ a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue _ (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int16 where
  toPrimitivePersistValue _ a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue _ (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int32 where
  toPrimitivePersistValue _ a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue _ (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Int64 where
  toPrimitivePersistValue _ a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue _ (PersistInt64 a) = a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word8 where
  toPrimitivePersistValue _ a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue _ (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word16 where
  toPrimitivePersistValue _ a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue _ (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word32 where
  toPrimitivePersistValue _ a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue _ (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Word64 where
  toPrimitivePersistValue _ a = PersistInt64 (fromIntegral a)
  fromPrimitivePersistValue _ (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Integer, received: " ++ show x)

instance PrimitivePersistField Double where
  toPrimitivePersistValue _ a = PersistDouble a
  fromPrimitivePersistValue _ (PersistDouble a) = a
  fromPrimitivePersistValue _ (PersistInt64 a) = fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Double, received: " ++ show x)

instance PrimitivePersistField Bool where
  toPrimitivePersistValue _ a = PersistBool a
  fromPrimitivePersistValue _ (PersistBool a) = a
  fromPrimitivePersistValue _ (PersistInt64 i) = i /= 0
  fromPrimitivePersistValue _ x = error $ "Expected Bool, received: " ++ show x

instance PrimitivePersistField Day where
  toPrimitivePersistValue _ a = PersistDay a
  fromPrimitivePersistValue _ (PersistDay a) = a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Day, received: " ++ show x)

instance PrimitivePersistField TimeOfDay where
  toPrimitivePersistValue _ a = PersistTimeOfDay a
  fromPrimitivePersistValue _ (PersistTimeOfDay a) = a
  fromPrimitivePersistValue _ x = readHelper x ("Expected TimeOfDay, received: " ++ show x)

instance PrimitivePersistField UTCTime where
  toPrimitivePersistValue _ a = PersistUTCTime a
  fromPrimitivePersistValue _ (PersistUTCTime a) = a
  fromPrimitivePersistValue _ x = readHelper x ("Expected UTCTime, received: " ++ show x)

instance PrimitivePersistField ZonedTime where
  toPrimitivePersistValue _ a = PersistZonedTime (ZT a)
  fromPrimitivePersistValue _ (PersistZonedTime (ZT a)) = a
  fromPrimitivePersistValue _ x = readHelper x ("Expected ZonedTime, received: " ++ show x)

instance (PrimitivePersistField a, NeverNull a) => PrimitivePersistField (Maybe a) where
  toPrimitivePersistValue p a = maybe PersistNull (toPrimitivePersistValue p) a
  fromPrimitivePersistValue _ PersistNull = Nothing
  fromPrimitivePersistValue p x = Just $ fromPrimitivePersistValue p x

instance (DbDescriptor db, PersistEntity v) => PrimitivePersistField (KeyForBackend db v) where
  toPrimitivePersistValue p (KeyForBackend a) = toPrimitivePersistValue p a
  fromPrimitivePersistValue p x = KeyForBackend (fromPrimitivePersistValue p x)

instance PrimitivePersistField a => PurePersistField a where
  toPurePersistValues = primToPurePersistValues
  fromPurePersistValues = primFromPurePersistValues

instance PrimitivePersistField a => SinglePersistField a where
  toSinglePersistValue = primToSinglePersistValue
  fromSinglePersistValue = primFromSinglePersistValue

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
  _ -> error $ "readHelper: " ++ errMessage
  where
    readHelper' str = case reads str of
      (a, _):_ -> a
      _        -> error $ "readHelper: " ++ errMessage

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

instance PersistField ZonedTime where
  persistName _ = "ZonedTime"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbDayTimeZoned

-- There is a weird bug in GHC 7.4.1 which causes program to hang. See ticket 7126.
-- instance (PersistField a, NeverNull a) => PersistField (Maybe a) where -- OK
-- instance (SinglePersistField a, NeverNull a) => PersistField (Maybe a) where -- HANGS
instance (PersistField a, NeverNull a) => PersistField (Maybe a) where
  persistName a = "Maybe" ++ delim : persistName ((undefined :: Maybe a -> a) a)
  toPersistValues Nothing = return (PersistNull:)
  toPersistValues (Just a) = toPersistValues a
  fromPersistValues [] = fail "fromPersistValues Maybe: empty list"
  fromPersistValues (PersistNull:xs) = return (Nothing, xs)
  fromPersistValues xs = fromPersistValues xs >>= \(x, xs') -> return (Just x, xs')
  dbType a = DbMaybe $ dbType ((undefined :: Maybe a -> a) a)

instance (PersistField a) => PersistField [a] where
  persistName a = "List" ++ delim : delim : persistName ((undefined :: [] a -> a) a)
  toPersistValues l = insertList l >>= toPersistValues
  fromPersistValues [] = fail "fromPersistValues []: empty list"
  fromPersistValues (x:xs) = phantomDb >>= \p -> getList (fromPrimitivePersistValue p x) >>= \l -> return (l, xs)
  dbType a = DbList (persistName a) $ dbType ((undefined :: [] a -> a) a)

instance PersistField () where
  persistName _ = "Unit" ++ [delim]
  toPersistValues _ = return id
  fromPersistValues xs = return ((), xs)
  dbType _ = DbEmbedded $ EmbeddedDef False []

instance (PersistField a, PersistField b) => PersistField (a, b) where
  persistName a = "Tuple2" ++ delim : delim : persistName ((undefined :: (a, b) -> a) a) ++ delim : persistName ((undefined :: (a, b) -> b) a)
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
  persistName a = "Tuple3" ++ delim : delim : persistName ((undefined :: (a, b, c) -> a) a) ++ delim : persistName ((undefined :: (a, b, c) -> b) a) ++ delim : persistName ((undefined :: (a, b, c) -> c) a)
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
  persistName a = "Tuple4" ++ delim : delim : persistName ((undefined :: (a, b, c, d) -> a) a) ++ delim : persistName ((undefined :: (a, b, c, d) -> b) a) ++ delim : persistName ((undefined :: (a, b, c, d) -> c) a) ++ delim : persistName ((undefined :: (a, b, c, d) -> d) a)
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
  persistName a = "Tuple5" ++ delim : delim : persistName ((undefined :: (a, b, c, d, e) -> a) a) ++ delim : persistName ((undefined :: (a, b, c, d, e) -> b) a) ++ delim : persistName ((undefined :: (a, b, c, d, e) -> c) a) ++ delim : persistName ((undefined :: (a, b, c, d, e) -> d) a) ++ delim : persistName ((undefined :: (a, b, c, d, e) -> e) a)
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
  persistName a = "KeyForBackend" ++ delim : persistName ((undefined :: KeyForBackend db v -> v) a)
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType a = dbType ((undefined :: KeyForBackend db v -> v) a)

instance (PersistEntity v, Constructor c, PersistField a) => Projection (Field v c a) db (RestrictionHolder v c) a where
  projectionExprs f = (ExprField (fieldChain f):)
  projectionResult _ = fromPersistValues

instance (PersistEntity v, Constructor c, PersistField a) => Projection (SubField v c a) db (RestrictionHolder v c) a where
  projectionExprs f = (ExprField (fieldChain f):)
  projectionResult _ = fromPersistValues

instance PersistField a => Projection (Expr db r a) db r a where
  projectionExprs e = (ExprRaw e:)
  projectionResult _ = fromPersistValues

instance (PersistEntity v, Constructor c, a ~ AutoKey v) => Projection (AutoKeyField v c) db (RestrictionHolder v c) a where
  projectionExprs f = (ExprField (fieldChain f):)
  projectionResult _ = fromPersistValues

instance (PersistEntity v, Constructor c) => Projection (c (ConstructorMarker v)) db (RestrictionHolder v c) v where
  projectionExprs c = ((map ExprField chains)++) where
    chains = map (\f -> (f, [])) $ constrParams constr
    e = entityDef ((undefined :: c (ConstructorMarker v) -> v) c)
    constr = constructors e !! phantomConstrNum c
  projectionResult c xs = toSinglePersistValue (phantomConstrNum c) >>= \cNum -> fromEntityPersistValues (cNum:xs)

instance (PersistEntity v, IsUniqueKey k, k ~ Key v (Unique u), r ~ RestrictionHolder v (UniqueConstr k))
      => Projection (u (UniqueMarker v)) db r k where
  projectionExprs u = ((map ExprField chains)++) where
    UniqueDef _ _ uFields = constrUniques constr !! uniqueNum ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u)
    chains = map (\f -> (f, [])) uFields
    constr = head $ constructors (entityDef ((undefined :: u (UniqueMarker v) -> v) u))
  projectionResult _ = pureFromPersistValue

instance (Projection a1 db r a1', Projection a2 db r a2') => Projection (a1, a2) db r (a1', a2') where
  projectionExprs (a1, a2) = projectionExprs a1 . projectionExprs a2
  projectionResult (a', b') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    return ((a, b), rest1)

instance (Projection a1 db r a1', Projection a2 db r a2', Projection a3 db r a3') => Projection (a1, a2, a3) db r (a1', a2', a3') where
  projectionExprs (a1, a2, a3) = projectionExprs a1 . projectionExprs a2 . projectionExprs a3
  projectionResult (a', b', c') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    return ((a, b, c), rest2)

instance (Projection a1 db r a1', Projection a2 db r a2', Projection a3 db r a3', Projection a4 db r a4') => Projection (a1, a2, a3, a4) db r (a1', a2', a3', a4') where
  projectionExprs (a1, a2, a3, a4) = projectionExprs a1 . projectionExprs a2 . projectionExprs a3 . projectionExprs a4
  projectionResult (a', b', c', d') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    return ((a, b, c, d), rest3)

instance (Projection a1 db r a1', Projection a2 db r a2', Projection a3 db r a3', Projection a4 db r a4', Projection a5 db r a5') => Projection (a1, a2, a3, a4, a5) db r (a1', a2', a3', a4', a5') where
  projectionExprs (a1, a2, a3, a4, a5) = projectionExprs a1 . projectionExprs a2 . projectionExprs a3 . projectionExprs a4 . projectionExprs a5
  projectionResult (a', b', c', d', e') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    return ((a, b, c, d, e), rest4)

instance (PersistEntity v, Constructor c, Projection (AutoKeyField v c) db r a') => Assignable (AutoKeyField v c) db r a'
instance (PersistEntity v, Constructor c, Projection (SubField v c a) db r a') => Assignable (SubField v c a) db r a'
instance (PersistEntity v, Constructor c, Projection (Field v c a) db r a') => Assignable (Field v c a) db r a'
instance (PersistEntity v, IsUniqueKey (Key v (Unique u)), Projection (u (UniqueMarker v)) db r a') => Assignable (u (UniqueMarker v)) db r a'

instance (PersistEntity v, Constructor c, a ~ AutoKey v) => FieldLike (AutoKeyField v c) db (RestrictionHolder v c) a where
  fieldChain a = chain where
    chain = ((name, dbType k), [])
    -- if it is Nothing, the name would not be used because the type will be () with no columns
    name = maybe "will_be_ignored" id $ constrAutoKeyName $ constructors e !! cNum
    k = (undefined :: AutoKeyField v c -> AutoKey v) a
    e = entityDef ((undefined :: AutoKeyField v c -> v) a)
    cNum = phantomConstrNum ((undefined :: AutoKeyField v c -> c (ConstructorMarker v)) a)

instance (PersistEntity v, Constructor c, PersistField a) => FieldLike (SubField v c a) db (RestrictionHolder v c) a where
  fieldChain (SubField a) = a

instance (PersistEntity v, Constructor c, PersistField a) => FieldLike (Field v c a) db (RestrictionHolder v c) a where
  fieldChain = entityFieldChain

instance (PersistEntity v, IsUniqueKey k, k ~ Key v (Unique u), r ~ RestrictionHolder v (UniqueConstr k))
      => FieldLike (u (UniqueMarker v)) db r k where
  fieldChain u = chain where
    UniqueDef _ _ uFields = constrUniques constr !! uniqueNum ((undefined :: u (UniqueMarker v) -> Key v (Unique u)) u)
    chain = (("will_be_ignored", DbEmbedded $ EmbeddedDef True uFields), [])
    constr = head $ constructors (entityDef ((undefined :: u (UniqueMarker v) -> v) u))
