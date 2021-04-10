{-# LANGUAGE BangPatterns, Safe #-}
module Geometry (
  Point (..), Segment (..), RichSegment (..),
  Line (..), Circle (..),
  dist,
  line,
  pointInCircle,
  linesIntersection,
  circumcircle,
  minimalEnclosingBall,
  (.+.), (.-.), (.*.), richSegment, richSegmentsIntersects, richSegmentContainsPoint
) where

import Random

import Control.Monad
import Data.Int
import Data.List
import Data.Maybe

data Point a = Point a a
--(Line a b c): a * x + b * y + c = 0
data Line a = Line a a a
data Segment a = Segment (Point a) (Point a)
data RichSegment a = RichSegment (Point a) (Point a) (Point a) (Point a) (Line a)
data Circle a = Circle (Point a) a

pop :: (a -> a -> a) -> Point a -> Point a -> Point a
pop f (Point x' y') (Point x'' y'') = Point (f x' x'') (f y' y'')

(.+.) :: Num a => Point a -> Point a -> Point a
(.+.) p' p'' = pop (+) p' p''

(.-.) :: Num a => Point a -> Point a -> Point a
(.-.) p' p'' = pop (-) p' p''

(.*.) :: Num a => Point a -> a -> Point a
(.*.) (Point x y) c = Point (x * c) (y * c)

minp :: Ord a => Point a -> Point a -> Point a
minp p' p'' = pop min p' p''

maxp :: Ord a => Point a -> Point a -> Point a
maxp p' p'' = pop max p' p''

infixl 6 .+., .-.

line :: Num a => Point a -> Point a -> Line a
line (Point x' y') (Point x'' y'') = Line a b c
  where
    a = y'' - y'
    b = x' - x''
    c = negate $ a * x' + b * y'

linePointSignum :: Num a => Line a -> Point a -> a
linePointSignum (Line a b c) (Point x y) = signum $ a * x + b * y + c

richSegment :: Num a => Ord a => Segment a -> RichSegment a
richSegment (Segment p' p'') = RichSegment p' p'' (minp p' p'') (maxp p' p'') (line p' p'')

richSegmentContainsPoint :: Num a => Ord a => RichSegment a -> Point a -> Bool
richSegmentContainsPoint (RichSegment _ _ (Point ux uy) (Point vx vy) l) p@(Point x y) =
  linePointSignum l p == 0 && ux <= x && x <= vx && uy <= y && y <= vy

richSegmentsIntersects :: Num a => Ord a => RichSegment a -> RichSegment a -> Bool
richSegmentsIntersects (RichSegment p1' p2' u' v' l') (RichSegment p1'' p2'' u'' v'' l'') =
  x' <= x'' && y' <= y'' && sign1' * sign2' <= 0 && sign1'' * sign2'' <= 0
  where
    (Point x' y')  = maxp u' u''
    (Point x'' y'')  = minp v' v''
    sign1' = linePointSignum l'' p1'
    sign2' = linePointSignum l'' p2'
    sign1'' = linePointSignum l' p1''
    sign2'' = linePointSignum l' p2''

dist :: Point Double -> Point Double -> Double
dist p' p'' = let (Point x y) =  p' .-. p'' in sqrt $ x * x + y * y

pointInCircle :: Point Double -> Circle Double -> Double -> Bool
pointInCircle p (Circle c r) eps = dist p c - r < eps

linesIntersection :: Line Double -> Line Double -> Double -> Maybe (Point Double)
linesIntersection (Line a1 b1 c1) (Line a2 b2 c2) eps
  | abs d < eps = Nothing
  | otherwise = Just (p .*. (1.0 / d))
  where
    d = a1 * b2 - a2 * b1
    p = Point (c2 * b1 - c1 * b2) (a2 * c1 - a1 * c2)

circle :: Point Double -> Point Double -> Circle Double
circle p1@(Point x1 y1) (Point x2 y2) = Circle p (dist p p1)
  where
    p = Point x y
    !x = (x1 + x2) * 0.5
    !y = (y1 + y2) * 0.5

circumcircle :: Point Double -> Point Double -> Point Double -> Double -> Maybe (Circle Double)
circumcircle p1 p2 p3 eps = liftM (\ p -> Circle p (dist p p1)) $ linesIntersection (cp p1 p2) (cp p2 p3) eps
  where
    cp :: Point Double -> Point Double -> Line Double
    cp q1 q2 = Line a b $ negate $ a * xm + b * ym
      where
        (Point xm ym) = (q1 .+. q2) .*. 0.5
        (Point a b) = q2 .-. q1

minimalEnclosingBallP2 :: [Point Double] -> Point Double -> Point Double -> Double -> Circle Double
minimalEnclosingBallP2 q q1 q2 eps = foldl' f (circle q1 q2) q
  where
    f c p = if pointInCircle p c eps then c else fromJust $ circumcircle q1 q2 p eps

minimalEnclosingBallP1 :: [Point Double] -> Point Double -> [Int32] -> Double -> (Circle Double, [Int32])
minimalEnclosingBallP1 q q1 rnds eps = (fst $ foldl' f ( circle q1 p', [p']) ps, rnds')
  where
    ( (p' : ps), rnds') = randomShuffle q rnds
    f (c, l) p = if pointInCircle p c eps then (c, p:l) else (minimalEnclosingBallP2 l q1 p eps, p:l)

minimalEnclosingBall :: [Point Double] -> [Int32] -> Double -> Circle Double
minimalEnclosingBall q rnds eps = let (c, _, _) = foldl' f (circle p1 p2, [p1,p2], rnds') ps in c
  where
    ( (p1 : p2 : ps), rnds') = randomShuffle q rnds
    f (c, l, r) p = if pointInCircle p c eps then (c, p:l, r) else let (c', r') = minimalEnclosingBallP1 l p r eps in (c', p:l, r')
