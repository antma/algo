{-# LANGUAGE FlexibleContexts, Safe #-}
module DisjointSet (
  disjointSetInit, disjointSetFind, disjointSetMerge
) where

import Control.Monad.ST
import Data.Array.ST.Safe

disjointSetInit :: Int -> ST s (STUArray s Int Int, STUArray s Int Int)
disjointSetInit n = do
  p <- newListArray (0, n) [0 .. n] :: ST s (STUArray s Int Int)
  h <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
  return (p, h)

disjointSetFind :: (MArray a Int m) => (a Int Int, t) -> Int -> m Int
disjointSetFind d@(p, _) x = do
  px <- readArray p x
  if px == x then return x
  else do
    px <- disjointSetFind d px
    writeArray p x px
    return px

disjointSetMerge :: (MArray a Int m) => (a Int Int, a Int Int) -> Int -> Int -> m (Bool)
disjointSetMerge d@(p, h) i j = do
  i' <- disjointSetFind d i
  j' <- disjointSetFind d j
  if i' /= j' then do
    hi <- readArray h i'
    hj <- readArray h j'
    if hi < hj then writeArray p i' j'
    else if hi > hj then writeArray p j' i'
    else do
      writeArray p i' j'
      writeArray h j' (succ hj)
    return True
  else return False
