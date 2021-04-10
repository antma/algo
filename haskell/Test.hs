module Main where

import Test.HUnit
import qualified TreapSet
import qualified Random
import qualified Primes
import qualified Geometry
import Data.Int
import Data.Maybe

-------------------- Geometry --------------------
geometry_tests :: [Test]
geometry_tests = [_test1, _test2, _test3, _test4]
  where
    p1 = Geometry.Point 0.0 0.0
    p2 = Geometry.Point 1.0 1.0
    p3 = Geometry.Point 1.0 0.0
    p4 = Geometry.Point 0.0 1.0
    p6 = Geometry.Point 0.5 0.5
    l1 = Geometry.line p1 p2
    l2 = Geometry.line p3 p4
    l3 = Geometry.line p1 p4
    l4 = Geometry.line p2 p3
    eps = 1e-10
    (Just p5) = Geometry.linesIntersection l1 l2 eps
    _test1 = TestCase (assertBool "two lines intersected" (abs (Geometry.dist p5 p6) < 1e-6))
    _test2 = TestCase (assertBool "two parallel lines" (isNothing $ Geometry.linesIntersection l3 l4 eps))
    s1 = Geometry.richSegment $ Geometry.Segment p1 p2
    s2 = Geometry.richSegment $ Geometry.Segment p3 p4
    Just (Geometry.Circle p7 r7) = Geometry.circumcircle p1 p3 p4 eps
    _test3 = TestCase (assertBool "segment intersected" (Geometry.richSegmentsIntersects s1 s2))
    _test4 = TestCase (assertBool "circumcircle" $ abs (r7 - 0.5 * sqrt 2.0) < eps && (abs (Geometry.dist p7 (Geometry.Point 0.5 0.5))< 1e-6))

maket :: [Int32] -> Int32 -> TreapSet.TreapSet
maket l seed = foldl (\acc (i, j) -> TreapSet.tsInsert i j acc) TreapSet.Nil $ zip l (Random.randoms seed)

test1 :: Test
test1 = TestCase (assertEqual "" (map (\i -> TreapSet.tsCount i t) [1,2,3,4]) [0,1,2,3])
  where
    t = maket [1,2,3] 7777

test2 = TestCase (assertBool "check heap (maximum)" (y >= yl && y >= yr))
  where
    (TreapSet.Node _ y _ l r) = maket [1 .. 100] 7
    (TreapSet.Node _ yl _ _ _) = l
    (TreapSet.Node _ yr _ _ _) = r

test3 = TestCase (assertBool "check sumOfDivisors" (Primes.sumOfDivisors [2] 6 == 12))

tests :: Test
tests = TestList $ [ TestLabel "test1" test1,
                     TestLabel "test2" test2,
                     TestLabel "test3" test3
                   ] ++ geometry_tests

main :: IO Counts
main = do runTestTT tests
