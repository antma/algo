module Main where

import Test.HUnit
import qualified TreapSet
import qualified Random
import Data.Int

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


tests :: Test
tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2]

main :: IO Counts
main = do runTestTT tests
