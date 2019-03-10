{-# LANGUAGE FlexibleContexts, Safe #-}
module DisjointSet (
  disjointSetInit, disjointSetFind, disjointSetMerge
) where

import Control.Monad.ST
import Data.Array.ST.Safe
import Data.Int

type DisjointSet s = (STUArray s Int Int, STUArray s Int Int32)

disjointSetInit :: Int -> ST s (DisjointSet s)
disjointSetInit n = do
  let n' = pred n
  p <- newListArray (0, n') [0 .. n'] :: ST s (STUArray s Int Int)
  h <- newArray (0, n') 0 :: ST s (STUArray s Int Int32)
  return (p, h)

disjointSetFind :: DisjointSet s -> Int -> ST s Int
disjointSetFind d@(p, _) x = do
  px <- readArray p x
  if px == x then return x
  else do
    px <- disjointSetFind d px
    writeArray p x px
    return px

disjointSetMerge :: DisjointSet s -> Int -> Int -> ST s Bool
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
