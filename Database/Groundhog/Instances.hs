{-# LANGUAGE TypeFamilies, GADTs, TypeSynonymInstances, OverlappingInstances, FlexibleInstances, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Groundhog.Instances (Selector(..)) where

import Control.Monad (liftM)

import Database.Groundhog.Core
import Database.Groundhog.Generic (failMessage)

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
  
instance SinglePersistField String where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField T.Text where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField ByteString where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Int where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Int8 where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Int16 where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Int32 where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Int64 where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Word8 where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Word16 where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Word32 where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Word64 where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Double where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Bool where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField Day where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField TimeOfDay where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance SinglePersistField UTCTime where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance PersistEntity a => SinglePersistField (Key a) where
  toSinglePersistValue = return . toPrim
  fromSinglePersistValue = return . fromPrim

instance (SinglePersistField a, NeverNull a) => SinglePersistField (Maybe a) where
  toSinglePersistValue Nothing = return PersistNull
  toSinglePersistValue (Just a) = toSinglePersistValue a
  fromSinglePersistValue PersistNull = return Nothing
  fromSinglePersistValue a = liftM Just $ fromSinglePersistValue a

instance PurePersistField String where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField T.Text where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField ByteString where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Int where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Int8 where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Int16 where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Int32 where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Int64 where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Word8 where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Word16 where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Word32 where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Word64 where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Double where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Bool where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField Day where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField TimeOfDay where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField UTCTime where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PersistEntity a => PurePersistField (Key a) where
  toPurePersistValues a = (toPrim a:)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance (PrimitivePersistField a, NeverNull a) => PurePersistField (Maybe a) where
  toPurePersistValues a = (maybe PersistNull toPrim a:)
  fromPurePersistValues (PersistNull:xs) = (Nothing, xs)
  fromPurePersistValues (x:xs) = (fromPrim x, xs)
  fromPurePersistValues xs = (\a -> error (failMessage a xs) `asTypeOf` (a, xs)) undefined

instance PurePersistField () where
  toPurePersistValues _ = id
  fromPurePersistValues xs = ((), xs)

instance (PurePersistField a, PurePersistField b) => PurePersistField (a, b) where
  toPurePersistValues (a, b) = toPurePersistValues a . toPurePersistValues b
  fromPurePersistValues xs = let
    (a, rest0) = fromPurePersistValues xs
    (b, rest1) = fromPurePersistValues rest0
    in ((a, b), rest1)

instance (PurePersistField a, PurePersistField b, PurePersistField c) => PurePersistField (a, b, c) where
  toPurePersistValues (a, b, c) = toPurePersistValues a . toPurePersistValues b . toPurePersistValues c
  fromPurePersistValues xs = let
    (a, rest0) = fromPurePersistValues xs
    (b, rest1) = fromPurePersistValues rest0
    (c, rest2) = fromPurePersistValues rest1
    in ((a, b, c), rest2)
  
instance (PurePersistField a, PurePersistField b, PurePersistField c, PurePersistField d) => PurePersistField (a, b, c, d) where
  toPurePersistValues (a, b, c, d) = toPurePersistValues a . toPurePersistValues b . toPurePersistValues c . toPurePersistValues d
  fromPurePersistValues xs = let
    (a, rest0) = fromPurePersistValues xs
    (b, rest1) = fromPurePersistValues rest0
    (c, rest2) = fromPurePersistValues rest1
    (d, rest3) = fromPurePersistValues rest2
    in ((a, b, c, d), rest3)
  
instance (PurePersistField a, PurePersistField b, PurePersistField c, PurePersistField d, PurePersistField e) => PurePersistField (a, b, c, d, e) where
  toPurePersistValues (a, b, c, d, e) = toPurePersistValues a . toPurePersistValues b . toPurePersistValues c . toPurePersistValues d . toPurePersistValues e
  fromPurePersistValues xs = let
    (a, rest0) = fromPurePersistValues xs
    (b, rest1) = fromPurePersistValues rest0
    (c, rest2) = fromPurePersistValues rest1
    (d, rest3) = fromPurePersistValues rest2
    (e, rest4) = fromPurePersistValues rest3
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
  toPrim = PersistString
  fromPrim (PersistString s) = s
  fromPrim (PersistByteString bs) = T.unpack $ T.decodeUtf8With T.lenientDecode bs
  fromPrim (PersistInt64 i) = show i
  fromPrim (PersistDouble d) = show d
  fromPrim (PersistDay d) = show d
  fromPrim (PersistTimeOfDay d) = show d
  fromPrim (PersistUTCTime d) = show d
  fromPrim (PersistBool b) = show b
  fromPrim PersistNull = error "Unexpected null"

instance PrimitivePersistField T.Text where
  toPrim = PersistString . T.unpack
  fromPrim (PersistByteString bs) = T.decodeUtf8With T.lenientDecode bs
  fromPrim x = T.pack $ fromPrim x

instance PrimitivePersistField ByteString where
  toPrim = PersistByteString
  fromPrim (PersistByteString a) = a
  fromPrim x = T.encodeUtf8 . T.pack $ fromPrim x

instance PrimitivePersistField Int where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Int8 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Int16 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Int32 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Int64 where
  toPrim = PersistInt64
  fromPrim (PersistInt64 a) = a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Word8 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Word16 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Word32 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Word64 where
  toPrim = PersistInt64 . fromIntegral
  fromPrim (PersistInt64 a) = fromIntegral a
  fromPrim x = error $ "Expected Integer, received: " ++ show x

instance PrimitivePersistField Double where
  toPrim = PersistDouble
  fromPrim (PersistDouble a) = a
  fromPrim x = error $ "Expected Double, received: " ++ show x

instance PrimitivePersistField Bool where
  toPrim = PersistBool
  fromPrim (PersistBool a) = a
  fromPrim (PersistInt64 i) = i /= 0
  fromPrim x = error $ "Expected Bool, received: " ++ show x

instance PrimitivePersistField Day where
  toPrim = PersistDay
  fromPrim (PersistDay a) = a
  fromPrim x = readHelper x ("Expected Day, received: " ++ show x)

instance PrimitivePersistField TimeOfDay where
  toPrim = PersistTimeOfDay
  fromPrim (PersistTimeOfDay a) = a
  fromPrim x = readHelper x ("Expected TimeOfDay, received: " ++ show x)

instance PrimitivePersistField UTCTime where
  toPrim = PersistUTCTime
  fromPrim (PersistUTCTime a) = a
  fromPrim x = readHelper x ("Expected UTCTime, received: " ++ show x)

instance PersistEntity a => PrimitivePersistField (Key a) where
  toPrim (Key a) = PersistInt64 a
  fromPrim (PersistInt64 a) = Key a
  fromPrim x = error $ "Expected Integer(entity key), received: " ++ show x

instance (PrimitivePersistField a, NeverNull a) => PrimitivePersistField (Maybe a) where
  toPrim = maybe PersistNull toPrim
  fromPrim PersistNull = Nothing
  fromPrim x = Just $ fromPrim x

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
instance NeverNull (Key a)

instance Expression (Expr v c a) where
  type FuncE (Expr v c a) v' c' = (v ~ v', c ~ c')
  type FuncA (Expr v c a) = a
  wrap = id

instance PersistEntity v => Expression (Field v c a) where
  type FuncE (Field v c a) v' c' = (v ~ v', c ~ c')
  type FuncA (Field v c a) = a
  wrap = ExprField

instance PersistEntity v => Expression (SubField v c a) where
  type FuncE (SubField v c a) v' c' = (v ~ v', c ~ c')
  type FuncA (SubField v c a) = a
  wrap = ExprField

instance PersistEntity v => Expression (Arith v c a) where
  type FuncE (Arith v c a) v' c' = (v ~ v', c ~ c')
  type FuncA (Arith v c a) = a
  wrap = ExprArith

instance (Expression a, PrimitivePersistField a, NeverNull a) => Expression (Maybe a) where
  type FuncE (Maybe a) v c = ()
  type FuncA (Maybe a) = (Maybe (FuncA a))
  wrap = ExprPure

instance PersistEntity a => Expression (Key a) where
  type FuncE (Key a) v c = (); type FuncA (Key a) = a
  wrap = ExprPure

instance Expression () where
  type FuncE () v c = ()
  type FuncA () = ()
  wrap = ExprPure

instance (PurePersistField a, PurePersistField b) => Expression (a, b) where
  type FuncE (a, b) v c = ()
  type FuncA (a, b) = (a, b)
  wrap = ExprPure

instance (PurePersistField a, PurePersistField b, PurePersistField c) => Expression (a, b, c) where
  type FuncE (a, b, c) v c = ()
  type FuncA (a, b, c) = (a, b, c)
  wrap = ExprPure

instance (PurePersistField a, PurePersistField b, PurePersistField c, PurePersistField d) => Expression (a, b, c, d) where
  type FuncE (a, b, c, d) v c = ()
  type FuncA (a, b, c, d) = (a, b, c, d)
  wrap = ExprPure

instance (PurePersistField a, PurePersistField b, PurePersistField c, PurePersistField d, PurePersistField e) => Expression (a, b, c, d, e) where
  type FuncE (a, b, c, d, e) v c = ()
  type FuncA (a, b, c, d, e) = (a, b, c, d, e)
  wrap = ExprPure

instance Expression Int where
  type FuncE Int v c = (); type FuncA Int = Int
  wrap = ExprPure

instance Expression Int8 where
  type FuncE Int8 v c = (); type FuncA Int8 = Int8
  wrap = ExprPure

instance Expression Int16 where
  type FuncE Int16 v c = (); type FuncA Int16 = Int16
  wrap = ExprPure

instance Expression Int32 where
  type FuncE Int32 v c = (); type FuncA Int32 = Int32
  wrap = ExprPure

instance Expression Int64 where
  type FuncE Int64 v c = (); type FuncA Int64 = Int64
  wrap = ExprPure

instance Expression Word8 where
  type FuncE Word8 v c = (); type FuncA Word8 = Word8
  wrap = ExprPure

instance Expression Word16 where
  type FuncE Word16 v c = (); type FuncA Word16 = Word16
  wrap = ExprPure

instance Expression Word32 where
  type FuncE Word32 v c = (); type FuncA Word32 = Word32
  wrap = ExprPure

instance Expression Word64 where
  type FuncE Word64 v c = (); type FuncA Word64 = Word64
  wrap = ExprPure

instance Expression String where
  type FuncE String v c = (); type FuncA String = String
  wrap = ExprPure

instance Expression ByteString where
  type FuncE ByteString v c = (); type FuncA ByteString = ByteString
  wrap = ExprPure

instance Expression T.Text where
  type FuncE T.Text v c = (); type FuncA T.Text = T.Text
  wrap = ExprPure

instance Expression Bool where
  type FuncE Bool v c = (); type FuncA Bool = Bool
  wrap = ExprPure

readHelper :: Read a => PersistValue -> String -> a
readHelper s errMessage = case s of
  PersistString str -> readHelper' str
  PersistByteString str -> readHelper' (unpack str)
  _ -> error errMessage
  where
    readHelper' str = case reads str of
      (a, _):_ -> a
      _        -> error errMessage

primFromPersistValue :: (PersistBackend m, PrimitivePersistField a) => [PersistValue] -> m (a, [PersistValue])
primFromPersistValue (x:xs) = return (fromPrim x, xs)
primFromPersistValue xs = (\a -> fail (failMessage a xs) >> return (a, xs)) undefined

instance PersistField ByteString where
  persistName _ = "ByteString"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbBlob

instance PersistField String where
  persistName _ = "String"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbString

instance PersistField T.Text where
  persistName _ = "Text"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbString

instance PersistField Int where
  persistName _ = "Int"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType a = if bitSize a == 32 then DbInt32 else DbInt64

instance PersistField Int8 where
  persistName _ = "Int8"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Int16 where
  persistName _ = "Int16"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Int32 where
  persistName _ = "Int32"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Int64 where
  persistName _ = "Int64"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt64

instance PersistField Word8 where
  persistName _ = "Word8"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Word16 where
  persistName _ = "Word16"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt32

instance PersistField Word32 where
  persistName _ = "Word32"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt64

instance PersistField Word64 where
  persistName _ = "Word64"
  toPersistValues a = return (toPrim a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbInt64

instance PersistField Double where
  persistName _ = "Double"
  toPersistValues a = return (PersistDouble a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbReal

instance PersistField Bool where
  persistName _ = "Bool"
  toPersistValues a = return (PersistBool a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbBool

instance PersistField Day where
  persistName _ = "Day"
  toPersistValues a = return (PersistDay a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbDay

instance PersistField TimeOfDay where
  persistName _ = "TimeOfDay"
  toPersistValues a = return (PersistTimeOfDay a:)
  fromPersistValues = primFromPersistValue
  dbType _ = DbTime

instance PersistField UTCTime where
  persistName _ = "UTCTime"
  toPersistValues a = return (PersistUTCTime a:)
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
  
instance (PersistEntity a) => PersistField (Key a) where
  persistName a = "Key$" ++ persistName ((undefined :: Key a -> a) a)
  toPersistValues (Key a) = return (PersistInt64 a:)
  fromPersistValues = primFromPersistValue
  dbType a = DbEntity $ entityDef ((undefined :: Key a -> a) a)

instance (PersistField a) => PersistField [a] where
  persistName a = "List$$" ++ persistName ((undefined :: [] a -> a) a)
  toPersistValues l = insertList l >>= toPersistValues
  fromPersistValues [] = fail "fromPersistValues []: empty list"
  fromPersistValues (x:xs) = getList (fromPrim x) >>= \l -> return (l, xs)
  dbType a = DbList (persistName a) $ dbType ((undefined :: [] a -> a) a)

instance PersistField () where
  persistName _ = "Unit$"
  toPersistValues _ = return id
  fromPersistValues xs = return ((), xs)
  dbType _ = DbEmbedded False []

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
  dbType a = DbEmbedded False [("val0", dbType ((undefined :: (a, b) -> a) a)), ("val1", dbType ((undefined :: (a, b) -> b) a))]
  
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
  dbType a = DbEmbedded False [("val0", dbType ((undefined :: (a, b, c) -> a) a)), ("val1", dbType ((undefined :: (a, b, c) -> b) a)), ("val2", dbType ((undefined :: (a, b, c) -> c) a))]
  
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
  dbType a = DbEmbedded False [("val0", dbType ((undefined :: (a, b, c, d) -> a) a)), ("val1", dbType ((undefined :: (a, b, c, d) -> b) a)), ("val2", dbType ((undefined :: (a, b, c, d) -> c) a)), ("val3", dbType ((undefined :: (a, b, c, d) -> d) a))]
  
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
  dbType a = DbEmbedded False [("val0", dbType ((undefined :: (a, b, c, d, e) -> a) a)), ("val1", dbType ((undefined :: (a, b, c, d, e) -> b) a)), ("val2", dbType ((undefined :: (a, b, c, d, e) -> c) a)), ("val3", dbType ((undefined :: (a, b, c, d, e) -> d) a)), ("val4", dbType ((undefined :: (a, b, c, d, e) -> e) a))]
