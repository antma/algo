{-# LANGUAGE BangPatterns, Safe #-}
module Geometry (
  Point,
  Line (..), Circle (..),
  dist,
  line,
  pointInCircle,
  linesIntersection,
  circumcircle,
  minimalEnclosingBall
) where

import Random

import Control.Monad
import Data.Int
import Data.List
import Data.Maybe

--(Line a b c): a * x + b * y = c
eps :: Double
eps = 1e-10

type Point = (Double, Double)
data Line = Line !Double !Double !Double
data Circle = Circle !Double !Double !Double

line :: Point -> Point -> Line
line (!x1, !y1) (!x2, !y2) = Line a b (a * x1 + b * y1)
  where
    !a = y2 - y1
    !b = x1 - x2

dist :: Point -> Point -> Double
dist (!x1, !y1) (!x2, !y2) = sqrt $! (x1 - x2) ^ 2 +  (y1 - y2) ^ 2

pointInCircle :: Point -> Circle -> Bool
pointInCircle p (Circle !x !y !r) = dist p (x, y) - r < eps

linesIntersection :: Line -> Line -> Maybe (Double, Double)
linesIntersection (Line a1 b1 c1) (Line a2 b2 c2)
  | abs d < eps = Nothing
  | otherwise = let !d' = 1.0 / d in Just ( d' * (c1 * b2 - c2 * b1), d' * (a1 * c2 - a2 * c1))
  where
    !d = a1 * b2 - a2 * b1

circle :: Point -> Point -> Circle
circle p1@(!x1, !y1) (!x2, !y2) = Circle x y (dist (x, y) p1)
  where
    !x = (x1 + x2) * 0.5
    !y = (y1 + y2) * 0.5

circumcircle ::  Point -> Point -> Point -> Maybe Circle
circumcircle p1 p2 p3 = liftM (\ p@(!x, !y) -> Circle x y (dist p p1)) $ linesIntersection (cp p1 p2) (cp p2 p3)
  where
    cp :: Point -> Point -> Line
    cp (!x1, !y1) (!x2, !y2) = Line a b (a * xm + b * ym)
      where
        !xm = (x1 + x2) * 0.5
        !ym = (y1 + y2) * 0.5
        !a = x2 - x1
        !b = y2 - y1

minimalEnclosingBallP2 :: [Point] -> Point -> Point -> Circle
minimalEnclosingBallP2 p q1 q2 = foldl' f (circle q1 q2) p
  where
    f :: Circle -> Point -> Circle
    f c p = if pointInCircle p c then c else fromJust $ circumcircle q1 q2 p

minimalEnclosingBallP1 :: [Point] -> Point -> [Int32] -> (Circle, [Int32])
minimalEnclosingBallP1 p q1 rnds = (fst $ foldl' f ( circle q1 p', [p']) ps, rnds')
  where
    ( (p' : ps), rnds') = randomShuffle p rnds
    f :: (Circle, [Point]) -> Point -> (Circle, [Point])
    f (c, l) p = if pointInCircle p c then (c, p:l) else (minimalEnclosingBallP2 l q1 p, p:l)

minimalEnclosingBall :: [Point] -> [Int32] -> Circle
minimalEnclosingBall p rnds = let (c, _, _) = foldl' f (circle p1 p2, [p1,p2], rnds') ps in c
  where
    ( (p1 : p2 : ps), rnds') = randomShuffle p rnds
    f :: (Circle, [Point], [Int32]) -> Point -> (Circle, [Point], [Int32])
    f (c, l, r) p = if pointInCircle p c then (c, p:l, r) else let (c', r') = minimalEnclosingBallP1 l p r in (c', p:l, r')
