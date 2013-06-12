module Database.Groundhog.Postgresql.Geometry
  (
    Point(..)
  , Line(..)
  , Lseg(..)
  , Box(..)
  , Path(..)
  , Polygon(..)
  , Circle(..)
  ) where

import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Instances ()

import Control.Applicative
import Data.Attoparsec.Char8

data Point = Point Double Double deriving (Eq, Show)
-- | It is not fully implemented in PostgreSQL yet. It is kept just to match all geometric types.
data Line = Line Point Point deriving (Eq, Show)
data Lseg = Lseg Point Point deriving (Eq, Show)
data Box = Box Point Point deriving (Eq, Show)
data Path = ClosedPath [Point]
          | OpenPath [Point] deriving (Eq, Show)
data Polygon = Polygon [Point] deriving (Eq, Show)
data Circle = Circle Point Double deriving (Eq, Show)

-- select o.oprname, o.oprkind, tl.typname as oprleft, tr.typname as oprright, tres.typname as oprresult, o.oprcode, ocom.oprname as oprcom, oneg.oprname as oprnegate from pg_operator o inner join pg_type tl on o.oprleft = tl.oid inner join pg_type tr on o.oprright = tr.oid inner join pg_type tres on o.oprresult = tres.oid left join pg_operator ocom on o.oprcom = ocom.oid left join pg_operator oneg on o.oprnegate = oneg.oid where tl.typname in ('point', 'line', 'lseg', 'box', 'path', 'polygon', 'circle') order by o.oprname, oprleft;

parseHelper :: Parser a -> PersistValue -> a
parseHelper p (PersistByteString bs) = either error id $ parseOnly p bs
parseHelper _ a = error $ "parseHelper: expected PersistByteString, got " ++ show a

pair :: (a -> a -> b) -> Char -> Char -> Parser a -> Parser b
pair f open close p = f <$> (char open *> p <* char ',') <*> p <* char close

point :: Parser Point
point = pair Point '(' ')' double

points :: Parser [Point]
points = point `sepBy1` char ','

instance PrimitivePersistField Point where
  toPrimitivePersistValue _ (Point x y) = PersistString $ show (x, y)
  fromPrimitivePersistValue _ = parseHelper point

instance PersistField Point where
  persistName _ = "Point"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbOther $ OtherTypeDef $ const "point"

instance PrimitivePersistField Line where
  toPrimitivePersistValue _ (Line (Point x1 y1) (Point x2 y2)) = PersistString $ show ((x1, y1), (x2, y2))
  fromPrimitivePersistValue _ = error "fromPrimitivePersistValue Line is not supported yet"

instance PersistField Line where
  persistName _ = "Line"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbOther $ OtherTypeDef $ const "line"

instance PrimitivePersistField Lseg where
  toPrimitivePersistValue _ (Lseg (Point x1 y1) (Point x2 y2)) = PersistString $ show ((x1, y1), (x2, y2))
  fromPrimitivePersistValue _ = parseHelper $ pair Lseg '[' ']' point

instance PersistField Lseg where
  persistName _ = "Lseg"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbOther $ OtherTypeDef $ const "lseg"

instance PrimitivePersistField Box where
  toPrimitivePersistValue _ (Box (Point x1 y1) (Point x2 y2)) = PersistString $ show ((x1, y1), (x2, y2))
  fromPrimitivePersistValue _ = parseHelper $ Box <$> (point <* char ',') <*> point

instance PersistField Box where
  persistName _ = "Box"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbOther $ OtherTypeDef $ const "box"

showPath :: Char -> Char -> [Point] -> ShowS
showPath open close []     s = open : close : s
showPath open close (x:xs) s = open : showPoint x (showl xs)
  where
    showl []     = close : s
    showl (y:ys) = ',' : showPoint y (showl ys)

showPoint :: Point -> ShowS
showPoint (Point x y) = shows (x, y)

instance PrimitivePersistField Path where
  toPrimitivePersistValue _ path = PersistString $ case path of
    ClosedPath ps -> showPath '(' ')' ps ""
    OpenPath ps -> showPath '[' ']' ps ""
  fromPrimitivePersistValue _ = parseHelper $ path' ClosedPath '(' ')' <|> path' OpenPath '[' ']' where
    path' f open close = f <$> (char open *> points <* char close)

instance PersistField Path where
  persistName _ = "Path"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbOther $ OtherTypeDef $ const "path"

instance PrimitivePersistField Polygon where
  toPrimitivePersistValue _ (Polygon ps) = PersistString $ showPath '(' ')' ps ""
  fromPrimitivePersistValue _ = parseHelper $ Polygon <$> (char '(' *> points <* char ')')

instance PersistField Polygon where
  persistName _ = "Polygon"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbOther $ OtherTypeDef $ const "polygon"

instance PrimitivePersistField Circle where
  toPrimitivePersistValue _ (Circle (Point x1 y1) r) = PersistString $ show ((x1, y1), r)
  fromPrimitivePersistValue _ = parseHelper $ Circle <$> (char '<' *> point) <* char ',' <*> double <* char '>'

instance PersistField Circle where
  persistName _ = "Circle"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbOther $ OtherTypeDef $ const "circle"
