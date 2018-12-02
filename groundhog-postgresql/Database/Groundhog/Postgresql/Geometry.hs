{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
module Database.Groundhog.Postgresql.Geometry
  (
    Point(..)
  , Line(..)
  , Lseg(..)
  , Box(..)
  , Path(..)
  , Polygon(..)
  , Circle(..)
  , (+.)
  , (-.)
  , (*.)
  , (/.)
  , (#)
  , (##)
  , (<->)
  , (&&)
  , (<<)
  , (>>)
  , (&<)
  , (&>)
  , (<<|)
  , (|>>)
  , (&<|)
  , (|&>)
  , (<^)
  , (>^)
  , (?#)
  , (?-)
  , (?|)
  , (?-|)
  , (?||)
  , (@>)
  , (<@)
  , (~=)
  ) where

import Prelude hiding ((&&), (>>))

import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Generic
import Database.Groundhog.Generic.Sql
import Database.Groundhog.Instances ()

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

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
  toPrimitivePersistValue (Point x y) = toPrimitivePersistValue $ show (x, y)
  fromPrimitivePersistValue = parseHelper point

instance PersistField Point where
  persistName _ = "Point"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef $ [Left "point"]) False Nothing Nothing

instance PrimitivePersistField Line where
  toPrimitivePersistValue (Line (Point x1 y1) (Point x2 y2)) = toPrimitivePersistValue $ show ((x1, y1), (x2, y2))
  fromPrimitivePersistValue = error "fromPrimitivePersistValue Line is not supported yet"

instance PersistField Line where
  persistName _ = "Line"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef $ [Left "line"]) False Nothing Nothing

instance PrimitivePersistField Lseg where
  toPrimitivePersistValue (Lseg (Point x1 y1) (Point x2 y2)) = toPrimitivePersistValue $ show ((x1, y1), (x2, y2))
  fromPrimitivePersistValue = parseHelper $ pair Lseg '[' ']' point

instance PersistField Lseg where
  persistName _ = "Lseg"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef $ [Left "lseg"]) False Nothing Nothing

instance PrimitivePersistField Box where
  toPrimitivePersistValue (Box (Point x1 y1) (Point x2 y2)) = toPrimitivePersistValue $ show ((x1, y1), (x2, y2))
  fromPrimitivePersistValue = parseHelper $ Box <$> (point <* char ',') <*> point

instance PersistField Box where
  persistName _ = "Box"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef $ [Left "box"]) False Nothing Nothing

showPath :: Char -> Char -> [Point] -> ShowS
showPath open close []     s = open : close : s
showPath open close (x:xs) s = open : showPoint x (showl xs)
  where
    showl []     = close : s
    showl (y:ys) = ',' : showPoint y (showl ys)

showPoint :: Point -> ShowS
showPoint (Point x y) = shows (x, y)

instance PrimitivePersistField Path where
  toPrimitivePersistValue path = toPrimitivePersistValue $ case path of
    ClosedPath ps -> showPath '(' ')' ps ""
    OpenPath ps -> showPath '[' ']' ps ""
  fromPrimitivePersistValue = parseHelper $ path' ClosedPath '(' ')' <|> path' OpenPath '[' ']' where
    path' f open close = f <$> (char open *> points <* char close)

instance PersistField Path where
  persistName _ = "Path"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef $ [Left "path"]) False Nothing Nothing

instance PrimitivePersistField Polygon where
  toPrimitivePersistValue (Polygon ps) = toPrimitivePersistValue $ showPath '(' ')' ps ""
  fromPrimitivePersistValue = parseHelper $ Polygon <$> (char '(' *> points <* char ')')

instance PersistField Polygon where
  persistName _ = "Polygon"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef $ [Left "polygon"]) False Nothing Nothing

instance PrimitivePersistField Circle where
  toPrimitivePersistValue (Circle (Point x1 y1) r) = toPrimitivePersistValue $ show ((x1, y1), r)
  fromPrimitivePersistValue = parseHelper $ Circle <$> (char '<' *> point) <* char ',' <*> double <* char '>'

instance PersistField Circle where
  persistName _ = "Circle"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef $ [Left "circle"]) False Nothing Nothing

class BoxLineLseg a
instance BoxLineLseg Box
instance BoxLineLseg Line
instance BoxLineLseg Lseg

class BoxCirclePolygon a
instance BoxCirclePolygon Box
instance BoxCirclePolygon Circle
instance BoxCirclePolygon Polygon

class BoxCirclePathPoint a
instance BoxCirclePathPoint Box
instance BoxCirclePathPoint Circle
instance BoxCirclePathPoint Path
instance BoxCirclePathPoint Point

class BoxCirclePointPolygon a
instance BoxCirclePointPolygon Box
instance BoxCirclePointPolygon Circle
instance BoxCirclePointPolygon Point
instance BoxCirclePointPolygon Polygon

class BoxPoint a
instance BoxPoint Box
instance BoxPoint Point

class LineLseg a
instance LineLseg Line
instance LineLseg Lseg

class Plus a b
instance Plus Box Point
instance Plus Circle Point
instance Plus Path Point
instance Plus Path Path
instance Plus Point Point

class Distance a b
instance Distance Box Box
instance Distance Circle Circle
instance Distance Circle Polygon
instance Distance Line Line
instance Distance Line Box
instance Distance Lseg Line
instance Distance Lseg Lseg
instance Distance Lseg Box
instance Distance Path Path
instance Distance Point Path
instance Distance Point Point
instance Distance Point Circle
instance Distance Point Line
instance Distance Point Box
instance Distance Point Lseg
instance Distance Polygon Polygon

class Contains a b
instance Contains Box Box
instance Contains Box Point
instance Contains Circle Circle
instance Contains Circle Point
instance Contains Path Point
instance Contains Polygon Polygon
instance Contains Polygon Point

class Contained a b
instance Contained Box Box
instance Contained Circle Circle
instance Contained Lseg Box
instance Contained Lseg Line
instance Contained Point Lseg
instance Contained Point Box
instance Contained Point Line
instance Contained Point Path
instance Contained Point Polygon
instance Contained Point Circle
instance Contained Polygon Polygon

class Closest a b
instance Closest Line Box
instance Closest Line Lseg
instance Closest Lseg Box
instance Closest Lseg Line
instance Closest Lseg Lseg
instance Closest Point Line
instance Closest Point Box
instance Closest Point Lseg

class Intersects a b
instance Intersects Box Box
instance Intersects Line Line
instance Intersects Line Box
instance Intersects Lseg Box
instance Intersects Lseg Line
instance Intersects Lseg Lseg
instance Intersects Path Path

psqlOperatorExpr :: (SqlDb db, Expression db r a, Expression db r b, PersistField c) => String -> a -> b -> Expr db r c
psqlOperatorExpr op x y = mkExpr $ operator 50 op x y

psqlOperatorCond :: (SqlDb db, Expression db r a, Expression db r b) => String -> a -> b -> Cond db r
psqlOperatorCond op x y = CondRaw $ operator 50 op x y


infixl 6 +.
infixl 6 -.
infixl 7 *.
infixl 7 /.
-- | Translation
--
-- @box '((0,0),(1,1))' + point '(2.0,0)' = box '(3,1),(2,0)'@
(+.) :: (SqlDb db, Plus a b, ExpressionOf db r x a, ExpressionOf db r y b) => x -> y -> Expr db r a
x +. y = mkExpr $ operator 60 "+" x y

-- | Translation
--
-- @box '((0,0),(1,1))' - point '(2.0,0)' = box '(-1,1),(-2,0)'@
(-.) :: (SqlDb db, BoxCirclePathPoint a, ExpressionOf db r x a, ExpressionOf db r y Point) => x -> y -> Expr db r a
x -. y = mkExpr $ operator 60 "-" x y

-- | Scaling/rotation
--
-- @box '((0,0),(1,1))' * point '(2.0,0)' = box '(2,2),(0,0)'@
(*.) :: (SqlDb db, BoxCirclePathPoint a, ExpressionOf db r x a, ExpressionOf db r y Point) => x -> y -> Expr db r a
x *. y = mkExpr $ operator 70 "*" x y

-- | Scaling/rotation
--
-- @box '((0,0),(2,2))' / point '(2.0,0)' = box '(1,1),(0,0)'@
(/.) :: (SqlDb db, BoxCirclePathPoint a, ExpressionOf db r x a, ExpressionOf db r y Point) => x -> y -> Expr db r a
x /. y = mkExpr $ operator 70 "/" x y

-- | Point or box of intersection
--
-- @lseg '((1,-1),(-1,1))' # '((1,1),(-1,-1))' = point '(0,0)'@
--
-- @box '((1,-1),(-1,1))' # '((1,1),(-1,-1))' = box '(1,1),(-1,-1)'@
(#) :: (SqlDb db, BoxLineLseg a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Expr db r a
(#) = psqlOperatorExpr "#"

-- | Closest point to first operand on second operand
--
-- @point '(0,0)' ## lseg '((2,0),(0,2))' = point '(1,1)'@
(##) :: (SqlDb db, Closest a b, ExpressionOf db r x a, ExpressionOf db r y b) => x -> y -> Expr db r Point
(##) = psqlOperatorExpr "##"

-- | Distance between
--
-- @circle '((0,0),1)' <-> circle '((5,0),1)' = 3@
(<->) :: (SqlDb db, Distance a b, ExpressionOf db r x a, ExpressionOf db r y b) => x -> y -> Expr db r Double
(<->) = psqlOperatorExpr "<->"

-- | Overlaps?
--
-- @box '((0,0),(1,1))' && box '((0,0),(2,2))' = true@
(&&) :: (SqlDb db, BoxCirclePolygon a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(&&) = psqlOperatorCond "&&"

-- | Is strictly left of?
--
-- @circle '((0,0),1)' << circle '((5,0),1)' = true@
(<<) :: (SqlDb db, BoxCirclePointPolygon a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(<<) = psqlOperatorCond "<<"

-- | Is strictly right of?
--
-- @circle '((5,0),1)' >> circle '((0,0),1)' = true@
(>>) :: (SqlDb db, BoxCirclePointPolygon a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(>>) = psqlOperatorCond ">>"

-- | Does not extend to the right of? box '((0,0),(1,1))' &< box '((0,0),(2,2))' = t
(&<) :: (SqlDb db, BoxCirclePolygon a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(&<) = psqlOperatorCond "&<"

-- | Does not extend to the left of?
--
-- @box '((0,0),(3,3))' &> box '((0,0),(2,2))' = true@
(&>) :: (SqlDb db, BoxCirclePolygon a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(&>) = psqlOperatorCond "&>"

-- | Is strictly below?
--
-- @box '((0,0),(3,3))' <<| box '((3,4),(5,5))' = true@
(<<|) :: (SqlDb db, BoxCirclePolygon a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(<<|) = psqlOperatorCond "<<|"

-- | Is strictly above?
--
-- @box '((3,4),(5,5))' |>> box '((0,0),(3,3))'@
(|>>):: (SqlDb db, BoxCirclePolygon a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(|>>) = psqlOperatorCond "|>>"

-- | Does not extend above?
--
-- @box '((0,0),(1,1))' &<| box '((0,0),(2,2))' = true@
(&<|):: (SqlDb db, BoxCirclePolygon a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(&<|) = psqlOperatorCond "&<|"

-- | Does not extend below?
--
-- @box '((0,0),(3,3))' |&> box '((0,0),(2,2))' = true@
(|&>) :: (SqlDb db, BoxCirclePolygon a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(|&>) = psqlOperatorCond "|&>"

-- | Is below (allows touching)?
--
-- @circle '((0,0),1)' <^ circle '((0,5),1)' = true@
(<^) :: (SqlDb db, BoxPoint a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(<^) = psqlOperatorCond "<^"

-- | Is above (allows touching)?
--
-- @circle '((0,5),1)' >^ circle '((0,0),1)' = true@
(>^) :: (SqlDb db, BoxPoint a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(>^) = psqlOperatorCond ">^"

-- | Intersects?
--
-- @lseg '((-1,0),(1,0))' ?# box '((-2,-2),(2,2))' = true@
(?#) :: (SqlDb db, Intersects a b, ExpressionOf db r x a, ExpressionOf db r y b) => x -> y -> Cond db r
(?#) = psqlOperatorCond "?#"

-- | Are horizontally aligned?
--
-- @point '(1,0)' ?- point '(0,0)' = true@
(?-) :: (SqlDb db, ExpressionOf db r x Point, ExpressionOf db r y Point) => x -> y -> Cond db r
(?-) = psqlOperatorCond "?-"

-- | Are vertically aligned?
--
-- @point '(0,1)' ?| point '(0,0)' = true@
(?|) :: (SqlDb db, ExpressionOf db r x Point, ExpressionOf db r y Point) => x -> y -> Cond db r
(?|) = psqlOperatorCond "?|"

-- | Is perpendicular?
--
-- @lseg '((0,0),(0,1))' ?-| lseg '((0,0),(1,0))' = true@
(?-|) :: (SqlDb db, LineLseg a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(?-|) = psqlOperatorCond "?-|"

-- | Are parallel?
--
-- @lseg '((-1,0),(1,0))' ?|| lseg '((-1,2),(1,2))' = true@
(?||) :: (SqlDb db, LineLseg a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(?||) = psqlOperatorCond "?||"

-- | Contains?
--
-- @circle '((0,0),2)' \@> point '(1,1)' = true@
(@>) :: (SqlDb db, Contains a b, ExpressionOf db r x a, ExpressionOf db r y b) => x -> y -> Cond db r
(@>) = psqlOperatorCond "@>"

-- | Contained in or on?
--
-- @point '(1,1)' <\@ circle '((0,0),2)' = true@
(<@) :: (SqlDb db, Contained a b, ExpressionOf db r x a, ExpressionOf db r y b) => x -> y -> Cond db r
(<@) = psqlOperatorCond "<@"

-- | Same as?
--
-- @polygon '((0,0),(1,1))' ~= polygon '((1,1),(0,0))' = true@
(~=) :: (SqlDb db, BoxCirclePointPolygon a, ExpressionOf db r x a, ExpressionOf db r y a) => x -> y -> Cond db r
(~=) = psqlOperatorCond "~="
