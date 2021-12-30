{-# LANGUAGE FlexibleContexts #-}

module Groundhog.Test.Specs.GeneralSpec where

import Control.Monad
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import qualified Data.Aeson as A
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Time as Time
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic (firstRow, phantomDb)
import Database.Groundhog.Generic.Sql (FloatingSqlDb (..), RenderConfig (..), RenderS (..), renderCond)
import Database.Groundhog.Generic.Sql.Functions
import Groundhog.Test.Types.Types
import Groundhog.Test.Util
import Test.Hspec (SpecWith, expectationFailure, it, shouldBe)

spec :: (PersistBackendConn conn, ExtractConnection cm conn, SqlDb conn, TryConnectionManager conn) => SpecWith cm
spec = do
  it "inserts values with a single constructor" $
    runDbConnIO $ do
      let val = Single "abc"
      migr val
      Just val @=?? (insert val >>= get)
      insert_ val
      [val, val] @=?? fmap (map snd) selectAll

  it "inserts values with multiple constructors" $
    runDbConnIO $ do
      let multi = Second "abc"
      migr multi
      Just multi @=?? (insert multi >>= get)
      insert_ multi
      [multi, multi] @=?? fmap (map snd) selectAll

  it "does count" $
    runDbConnIO $ do
      migr (undefined :: Multi String)
      insert_ (First 0 :: Multi String)
      insert_ (Second "abc")
      num <- countAll (undefined :: Multi String)
      liftIO $ num `shouldBe` 2
      num2 <- count $ SecondField ==. "abc"
      liftIO $ num2 `shouldBe` 1
      migr (undefined :: Single String)
      insert_ $ Single "abc"
      num3 <- count (SingleField ==. "abc")
      liftIO $ num3 `shouldBe` 1

  it "does update" $
    runDbConnIO $ do
      let val = Single ("abc", "def")
      migr val
      k <- insert val
      -- update columns using embedded data structure
      update [SingleField =. ("ghc", "qqq")] (SingleField ~> Tuple2_0Selector ==. "abc")
      val1 <- get k
      liftIO $ val1 `shouldBe` Just (Single ("ghc", "qqq"))
      -- update columns to the initial values using embedded data structure subfields
      update [SingleField ~> Tuple2_0Selector =. "abc", SingleField ~> Tuple2_1Selector =. "def"] (SingleField ==. ("ghc", "qqq"))
      val2 <- get k
      liftIO $ val2 `shouldBe` Just val

  it "does delete" $
    runDbConnIO $ do
      migr (undefined :: Multi String)
      k <- insert $ Second "abc"
      delete $ SecondField ==. "abc"
      Nothing @=?? (queryRaw' "SELECT * FROM \"Multi#String\" WHERE id=?" [toPrimitivePersistValue k] >>= firstRow)
      Nothing @=?? (queryRaw' "SELECT * FROM \"Multi#String#Second\" WHERE id=?" [toPrimitivePersistValue k] >>= firstRow)

  it "does deleteBy" $
    runDbConnIO $ do
      migr (undefined :: Multi String)
      k <- insert $ Second "abc"
      deleteBy k
      Nothing @=?? (queryRaw' "SELECT * FROM \"Multi#String\" WHERE id=?" [toPrimitivePersistValue k] >>= firstRow)
      Nothing @=?? (queryRaw' "SELECT * FROM \"Multi#String#Second\" WHERE id=?" [toPrimitivePersistValue k] >>= firstRow)

  it "does deleteAll" $
    runDbConnIO $ do
      let val = Second "abc"
      migr val
      insert_ val
      deleteAll val
      Nothing @=?? (queryRaw' "SELECT * FROM \"Multi#String\"" [] >>= firstRow)
      Nothing @=?? (queryRaw' "SELECT * FROM \"Multi#String#Second\"" [] >>= firstRow)

  it "replaces value with a single constructor" $
    runDbConnIO $ do
      -- we need Single to test that referenced value can be replaced
      let val = Single (Single "abc")
      migr val
      k <- insert val
      valueKey' <- queryRaw' "SELECT \"single\" FROM \"Single#Single#String\" WHERE id=?" [toPrimitivePersistValue k] >>= firstRow
      let valueKey = fromPrimitivePersistValue $ head $ fromJust valueKey'
      replace valueKey (Single "def")
      replaced <- get valueKey
      liftIO $ replaced `shouldBe` Just (Single "def")

  it "replaces value with multiple constructors" $
    runDbConnIO $ do
      migr (undefined :: Single (Multi String))
      -- we need Single to test that referenced value can be replaced
      k <- insert $ Single (Second "abc")
      valueKey' <- queryRaw' "SELECT \"single\" FROM \"Single#Multi#String\" WHERE id=?" [toPrimitivePersistValue k] >>= firstRow
      let valueKey = fromPrimitivePersistValue $ head $ fromJust valueKey'

      replace valueKey (Second "def")
      replaced <- get valueKey
      liftIO $ replaced `shouldBe` Just (Second "def")

      replace valueKey (First 5)
      replaced2 <- get valueKey
      liftIO $ replaced2 `shouldBe` Just (First 5)
      oldConstructor <- queryRaw' "SELECT * FROM \"Multi#String#Second\" WHERE id=?" [toPrimitivePersistValue valueKey] >>= firstRow
      liftIO $ oldConstructor `shouldBe` Nothing

  it "replaces a value by a unique constraint" $
    runDbConnIO $ do
      let val = UniqueKeySample 1 2 (Just 3)
          replaced = UniqueKeySample 1 3 Nothing
      migr val
      insert_ val
      replaceBy Unique_key_one_column replaced
      Just replaced @=?? getBy (Unique_key_one_columnKey 1)

  it "works with nullable values" $
    runDbConnIO $ do
      -- There is a weird bug in GHC 7.4.1 which causes program to hang if there is a type class constraint which is not used. See ticket 7126. It may arise with maybes
      -- instance (PersistField a, NeverNull a) => PersistField (Maybe a) where -- OK
      -- instance (SinglePersistField a, NeverNull a) => PersistField (Maybe a) where -- HANGS
      let val = Single (Just "abc")
      migr val
      Just val @=?? (insert val >>= get)

  it "works with tuples" $
    runDbConnIO $ do
      let val = Single ("abc", ("def", 5 :: Int))
      migr val
      k <- insert val
      val' <- get k
      liftIO $ val' `shouldBe` Just val

  it "works with a list of tuples" $
    runDbConnIO $ do
      let val = Single [("abc", 4 :: Int), ("def", 5)]
      migr val
      k <- insert val
      val' <- get k
      liftIO $ val' `shouldBe` Just val

  it "enforces a foreign key" $
    runDbConnIO $ do
      migr (undefined :: Single (Key (Single String) BackendSpecific))
      k <- insert $ Single "abc"
      insert_ $ Single k
      assertExc "Foreign key must prevent deletion" $ deleteBy k

  it "enforces a nullable foreign key" $
    runDbConnIO $ do
      migr (undefined :: Single (Maybe (Key (Single String) BackendSpecific)))
      k <- insert $ Single "abc"
      insert_ $ Single $ Just k
      assertExc "Foreign key must prevent deletion" $ deleteBy k

  it "makes queries with a Unique key" $
    runDbConnIO $ do
      let uVal = UniqueKeySample 1 2 (Just 3)
      let uKey = Unique_key_two_columnsKey 2 (Just 3)
      let val = Single uVal
      migr val
      k <- insert val
      Just val @=?? get k
      [uVal] @=?? select (Unique_key_two_columns ==. uKey)
      Left () @=?? insertByAll uVal
      -- check that constraints with nulls can be repeated
      insert_ $ UniqueKeySample 2 2 Nothing
      insert_ $ UniqueKeySample 3 2 Nothing
      void $ insertByAll $ UniqueKeySample 4 2 Nothing
      3 @=?? count (Unique_key_two_columns ==. Unique_key_two_columnsKey 2 Nothing)

  it "works with a reference to a composite unique key" $
    runDbConnIO $ do
      let uVal = UniqueKeySample 1 2 (Just 3)
      let val = HoldsUniqueKey (extractUnique uVal)
      migr val
      insert_ uVal
      insert_ val

  it "preserves Key with a PersistValue roundtrip" $
    runDbConnIO $ do
      let val = undefined :: Single (Key (Single String) BackendSpecific)
      migr val
      let val1 = Single "abc"
      SingleKey k <- insert val1
      kString <- fromSinglePersistValue k
      val' <- toSinglePersistValue (kString :: String) >>= get . SingleKey
      liftIO $ val' `shouldBe` Just val1

  it "preserves Key with a String roundtrip" $
    runDbConnIO $ do
      let val = Single "abc"
      migr val
      k <- insert val
      let stringKey = fromPrimitivePersistValue $ toPrimitivePersistValue k :: String
      result <- get $ fromPrimitivePersistValue $ toPrimitivePersistValue stringKey
      liftIO $ result `shouldBe` Just val

  it "selects with a condition on AutoKeyField" $
    runDbConnIO $ do
      let val = Single "abc"
      migr val
      k <- insert val
      result <- select $ AutoKeyField ==. k
      liftIO $ result `shouldBe` [val]

  it "selects with a condition on various key data types" $
    runDbConnIO $ do
      migr (undefined :: Keys)
      k <- insert $ Single ""
      void $ select $ RefDirectField ==. k ||. RefKeyField ==. k ||. RefDirectMaybeField ==. Just k ||. RefKeyMaybeField ==. Just k
      void $ select $ AutoKeyField ==. k

  it "roundtrips the number types" $
    runDbConnIO $ do
      migr (undefined :: Number)
      let minNumber = Number minBound minBound minBound minBound minBound minBound minBound minBound minBound
      let maxNumber = Number maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound
      Just minNumber @=?? (insert minNumber >>= get)
      Just maxNumber @=?? (insert maxNumber >>= get)

  it "roundtrips strings with the low-range characters" $
    runDbConnIO $ do
      let val = Single $ "\x0001\x0081\x0801\x1001" ++ ['\1' .. '\255']
      migr val
      k <- insert val
      val' <- get k
      liftIO $ val' `shouldBe` Just val

  it "roundtrips embedded data types" $
    runDbConnIO $ do
      let val1 = Single (EmbeddedSample "abc" (5, 6))
      migr val1
      k1 <- insert val1
      val1' <- get k1
      liftIO $ val1' `shouldBe` Just val1
      vals <- select $ SingleField ~> Embedded1Selector ==. "abc" &&. SingleField ~> Embedded2Selector ==. (5, 6) &&. SingleField ~> Embedded2Selector ~> Tuple2_0Selector ==. (5 :: Int)
      liftIO $ vals `shouldBe` [val1]
      let val2 = Single (EmbeddedSample "abc" (5, 6), "def")
      migr val2
      k2 <- insert val2
      val2' <- get k2
      liftIO $ val2' `shouldBe` Just val2

  it "roundtrips the time data types" $
    runDbConnIO $ do
      utcTime <- liftIO Time.getCurrentTime
      let dayTime = Time.timeToTimeOfDay $ Time.utctDayTime utcTime
      let day = Time.utctDay utcTime
      timeZone <- liftIO Time.getCurrentTimeZone
      let zonedTime = Time.utcToZonedTime (timeZone {Time.timeZoneName = ""}) utcTime
      let val = Single (utcTime, dayTime, day, zonedTime)
      migr val
      k <- insert val
      Just (Single (utcTime', dayTime', day', zonedTime')) <- get k
      liftIO $ (utcTime', dayTime', day') `shouldBe` (utcTime, dayTime, day)
      liftIO $ (Time.zonedTimeZone zonedTime', Time.zonedTimeToLocalTime zonedTime') `shouldBe` (Time.zonedTimeZone zonedTime, Time.zonedTimeToLocalTime zonedTime)

  it "roundtrips primitive data with enum and show/read converters" $
    runDbConnIO $ do
      let val = Single (Enum2, ShowRead "abc" 42)
      migr val
      Just val @=?? (insert val >>= get)

  it "roundtrips JSON for values and keys" $
    runDbConnIO $ do
      let val = Single (A.toJSON [1 :: Int, 2])
      migr val
      k <- insert val
      Just val @=?? get k

      -- test conversion of Key to JSON
      liftIO $ A.fromJSON (A.toJSON k) `shouldBe` A.Success k

  it "selects with various conditions" $
    runDbConnIO $ do
      migr (undefined :: Single (Int, String))
      let val1 = Single (5 :: Int, "abc")
      let val2 = Single (7 :: Int, "DEF")
      let val3 = Single (11 :: Int, "ghc")
      insert_ val1
      insert_ val2
      insert_ val3
      [val3] @=?? (select $ (SingleField ~> Tuple2_0Selector >. (5 :: Int)) `orderBy` [Asc (SingleField ~> Tuple2_1Selector)] `offsetBy` 1)
      [val2] @=?? (select $ (SingleField ~> Tuple2_0Selector >. (5 :: Int)) `orderBy` [Asc (SingleField ~> Tuple2_1Selector)] `limitTo` 1)
      [val2] @=?? (select $ (SingleField >=. (6 :: Int, "something") &&. SingleField ~> Tuple2_1Selector <. "ghc") `limitTo` 1)
      [val3] @=?? (select $ liftExpr (SingleField ~> Tuple2_0Selector) + 1 >. (10 :: Int))
      [val2] @=?? (select $ (SingleField ~> Tuple2_1Selector) `like` "%E%")
      [val2] @=?? (select $ lower (SingleField ~> Tuple2_1Selector) ==. "def")
      [val1, val2] @=?? (select $ ((SingleField ~> Tuple2_0Selector) `in_` [7 :: Int, 5]) `orderBy` [Asc (SingleField ~> Tuple2_0Selector)])
      [val1] @=?? (select $ CondEmpty `orderBy` [Asc SingleField] `limitTo` 1)
      [val1] @=?? (select $ CondEmpty `orderBy` [Asc SingleField, Desc SingleField] `limitTo` 1)
      ([] :: [Single (Int, String)]) @=?? (select $ Not CondEmpty)

  it "selects with DISTINCT" $
    runDbConnIO $ do
      let val1 = Single (5 :: Int, "abc")
          val2 = Single (6 :: Int, "def")
      migr val1
      insert_ val1
      insert_ val1
      insert_ val2
      [val1, val2] @=?? (select $ distinct $ CondEmpty `orderBy` [Asc $ SingleField ~> Tuple2_0Selector])

  it "selects with complex conditions" $
    runDbConnIO $ do
      proxy <- phantomDb
      let (===) :: (SqlDb (Conn m), MonadIO m, PrimitivePersistField a) => (String, [a]) -> Cond (Conn m) r -> m ()
          (query, vals) === cond = do
            let Just (RenderS q v) = renderCond (RenderConfig id) cond
            let equals = T.pack $ case backendName proxy of
                  "sqlite" -> " IS "
                  "postgresql" -> " IS NOT DISTINCT FROM "
                  "mysql" -> "<=>"
                  _ -> "="
                query' = Utf8 $ T.fromLazyText $ T.replace (T.pack " IS ") equals $ T.pack query
            liftIO $ (q, v []) `shouldBe` (query', map toPrimitivePersistValue vals)

      let intField f = f `asTypeOf` (undefined :: Field (Single (Int, Int)) c a)
          intNum = fromInteger :: SqlDb db => Integer -> Expr db r Int
      -- should cover all cases of renderCond comparison rendering
      ("single#val0=? AND single#val1=?", ["abc", "def"]) === (SingleField ==. ("abc", "def"))
      ("single#val0=single#val1", [] :: [Int]) === (intField SingleField ~> Tuple2_0Selector ==. SingleField ~> Tuple2_1Selector)
      ("single#val1=single#val0*(?+single#val0)", [5 :: Int]) === (intField SingleField ~> Tuple2_1Selector ==. liftExpr (SingleField ~> Tuple2_0Selector) * (5 + liftExpr (SingleField ~> Tuple2_0Selector)))

      ("?=? AND ?=?", [1, 2, 3, 4 :: Int]) === ((1 :: Int, 3 :: Int) ==. (2 :: Int, 4 :: Int) &&. SingleField ==. ()) -- SingleField ==. () is required to replace Any with a PersistEntity instance
      ("?<? OR ?<?", [1, 2, 3, 4 :: Int]) === ((1 :: Int, 3 :: Int) <. (2 :: Int, 4 :: Int) &&. SingleField ==. ())
      ("?=single#val0 AND ?=single#val1", [1, 2 :: Int]) === ((1 :: Int, 2 :: Int) ==. SingleField)
      ("?=single+?*?", [1, 2, 3 :: Int]) === ((1 :: Int) ==. liftExpr SingleField + 2 * 3)

      --  ("?-single=?", [1, 2 :: Int]) === (1 - liftExpr SingleField ==. (2 :: Int))
      ("?*single>single", [1 :: Int]) === (intNum 1 * liftExpr SingleField >. SingleField)
      --  ("?+single>=single-?", [1, 2 :: Int]) === (intNum 1 + liftExpr SingleField >=. liftExpr SingleField - 2)

      -- test parentheses
      ("NOT (NOT single=? OR ?=? AND ?=?)", [0, 1, 2, 3, 4 :: Int]) === Not (Not (SingleField ==. (0 :: Int)) ||. (1 :: Int, 3 :: Int) ==. (2 :: Int, 4 :: Int))
      ("single=? AND (?<? OR ?<?)", [0, 1, 2, 3, 4 :: Int]) === (SingleField ==. (0 :: Int) &&. (1 :: Int, 3 :: Int) <. (2 :: Int, 4 :: Int))
      ("NOT (single=? AND (single=single OR single<single))", [0 :: Int]) === Not (SingleField ==. (0 :: Int) &&. (SingleField ==. SingleField ||. SingleField <. SingleField))

      -- test empty conditions
      ("single#val0=? AND single#val1=?", ["abc", "def"]) === (SingleField ==. ("abc", "def") &&. (() ==. () ||. ((), ()) <. ((), ())))
      ("single#val0=? AND single#val1=?", ["abc", "def"]) === ((() ==. () ||. ((), ()) <. ((), ())) &&. SingleField ==. ("abc", "def"))

      -- test conditions used as expressions
      ("(?=?)=(?=?)", [1, 1, 0, 0] :: [Int]) === (((1 :: Int) ==. (1 :: Int)) ==. ((0 :: Int) ==. (0 :: Int)))

      -- test nullable expressions
      ("single#val0 IS ? AND single#val1=?", ["abc", "def"]) === (SingleField ==. (Just "abc", "def"))

  it "selects with comparison operators" $
    runDbConnIO $ do
      let val1 = Single (1 :: Int)
      let val2 = Single (2 :: Int)
      migr val1
      _ <- insert val1
      _ <- insert val2
      result1 <- select $ SingleField ==. (1 :: Int)
      liftIO $ result1 `shouldBe` [val1]
      result2 <- select $ SingleField /=. (1 :: Int)
      liftIO $ result2 `shouldBe` [val2]
      result3 <- select $ SingleField <. (2 :: Int)
      liftIO $ result3 `shouldBe` [val1]
      result4 <- select $ SingleField >. (1 :: Int)
      liftIO $ result4 `shouldBe` [val2]
      result5 <- select $ SingleField >=. (2 :: Int)
      liftIO $ result5 `shouldBe` [val2]
      result6 <- select $ SingleField <=. (1 :: Int)
      liftIO $ result6 `shouldBe` [val1]

  it "selects a projection of fields" $
    runDbConnIO $ do
      let val = Single ("abc", 5 :: Int)
      migr val
      k <- insert val
      result <- project (AutoKeyField, SingleConstructor, SingleField, SingleField ~> Tuple2_1Selector, SingleField ==. SingleField) ("" ==. "")
      liftIO $ result `shouldBe` [(k, val, ("abc", 5 :: Int), 5 :: Int, True)]
      let uVal = UniqueKeySample 1 2 (Just 3)
      migr uVal
      insert uVal
      result2 <- project Unique_key_two_columns ("" ==. "")
      liftIO $ result2 `shouldBe` [extractUnique uVal]

  it "selects a projection with string functions" $
    runDbConnIO $ do
      let val = Single "abc"
      migr val
      insert_ val
      result <- project ("hello " `append` upper SingleField) (() ==. ())
      liftIO $ result `shouldBe` ["hello ABC"]

  it "selects a projection with arithmetic expressions" $
    runDbConnIO $ do
      let val = Single (1 :: Int)
      migr val
      k <- insert val
      [(4, -4, 4)] @=?? project ((liftExpr SingleField + 1) * 2, liftExpr SingleField - 5, abs $ liftExpr SingleField - 5) (AutoKeyField ==. k)
      [(1, 0, -1)] @=?? project (signum $ liftExpr SingleField, signum $ liftExpr SingleField - 1, signum $ liftExpr SingleField - 10) (AutoKeyField ==. k)
      [(-6, -1)] @=?? project (quotRem (liftExpr SingleField - 20) 3) (AutoKeyField ==. k)
      [(-7, 2)] @=?? project (divMod (liftExpr SingleField - 20) 3) (AutoKeyField ==. k)

  it "roundtrips fields that have a string-based converter override" $
    runDbConnIO $ do
      let val = ConverterTest $ NotFieldInstance "abc"
      migr val
      Just val @=?? (insert val >>= get)

  it "handles exceptions and rollbacks" $ \c -> do
    result1 <- runTryDbConn success c
    checkRight result1 :: IO ()

    result2 <- runTryDbConn dbException c
    checkLeft result2 -- should be (Left error)
    result3 <- runTryDbConn throwException c
    checkLeft result3 -- should be (Left error)
  where
    dbException :: (MonadIO m, MonadFail m, PersistBackendConn conn) => TryAction TestException m conn ()
    dbException = do
      let val = UniqueKeySample 1 2 (Just 3)
      migr val
      insert val
      insert val -- should fail with uniqueness exception
    throwException :: (MonadIO m, MonadFail m, PersistBackendConn conn) => TryAction TestException m conn ()
    throwException = do
      void $ lift $ throwE TestException

    success :: (MonadIO m, MonadFail m, PersistBackendConn conn) => TryAction TestException m conn ()
    success = do
      pure ()

    -- These helpers are useful for Either a SomeException since the exception is not Eq and cannot be compared
    checkLeft :: MonadIO m => Either a b -> m ()
    checkLeft e = case e of
      Right _ -> liftIO $ expectationFailure "exception not caught"
      Left _ -> pure ()

    checkRight :: (MonadIO m, Show a) => Either a b -> m ()
    checkRight e = case e of
      Right _ -> pure ()
      Left err -> liftIO $ expectationFailure ("caught unexpected exception: " ++ show err)

floatingSpec :: (PersistBackendConn conn, ExtractConnection cm conn, FloatingSqlDb conn) => SpecWith cm
floatingSpec = do
  it "works with functions on floating numbers" $
    runDbConnIO $ do
      let val = Single (pi :: Double)
      migr val
      k <- insert val

      let shouldBeCloseTo ::
            (PersistBackend m, FloatingSqlDb (Conn m), MonadIO m) =>
            Expr (Conn m) (RestrictionHolder (Single Double) SingleConstructor) Double ->
            Double ->
            m ()
          shouldBeCloseTo expr expected = do
            actual <- fmap head $ project expr $ AutoKeyField ==. k
            unless (abs (expected - actual) < 0.0001) $
              liftIO $ actual `shouldBe` expected

      degrees SingleField `shouldBeCloseTo` 180
      radians (degrees SingleField) `shouldBeCloseTo` pi
      sin (liftExpr SingleField) `shouldBeCloseTo` 0
      cos (liftExpr SingleField) `shouldBeCloseTo` (-1)
      tan (liftExpr SingleField / 4) `shouldBeCloseTo` 1
      cot (liftExpr SingleField / 2) `shouldBeCloseTo` 0
      asin (sin $ liftExpr SingleField / 2) `shouldBeCloseTo` (pi / 2)
      acos (cos $ liftExpr SingleField) `shouldBeCloseTo` pi
      exp 2 `shouldBeCloseTo` exp 2
      sqrt (liftExpr SingleField) `shouldBeCloseTo` sqrt pi
      exp (liftExpr SingleField) `shouldBeCloseTo` exp pi
      (liftExpr SingleField ** 2) `shouldBeCloseTo` (pi ** 2)
      log (liftExpr SingleField) `shouldBeCloseTo` log pi
      logBase 3 (liftExpr SingleField) `shouldBeCloseTo` logBase 3 pi

      sinh (liftExpr SingleField) `shouldBeCloseTo` sinh pi
      tanh (liftExpr SingleField) `shouldBeCloseTo` tanh pi
      cosh (liftExpr SingleField) `shouldBeCloseTo` cosh pi
      asinh (liftExpr SingleField) `shouldBeCloseTo` asinh pi
      atanh (liftExpr SingleField / 4) `shouldBeCloseTo` atanh (pi / 4)
      acosh (liftExpr SingleField) `shouldBeCloseTo` acosh pi
