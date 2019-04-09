{-# LANGUAGE BangPatterns, Safe #-}
module Random (
  pmGen, randoms,
  randomPermutation,
  randomShuffle
) where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST.Safe
import Data.Int

pmGen :: Int32 -> Int32
pmGen x = fromIntegral $! mod ((16807::Int64) * fromIntegral x) 2147483647

randoms :: Int32 -> [Int32]
randoms = iterate pmGen

randomPermutation :: [Int32] -> Int -> ([Int32], [Int32])
randomPermutation rnds n = runST $ do
  let
    (r, t) = splitAt (n - 1) rnds
    !n32 = fromIntegral n :: Int32
  p <- newListArray (1, n) [1 .. n32] :: ST s (STUArray s Int Int32)
  forM_ (zip [n, pred n .. 2] r) $ \ (!i, !k) -> do
    let !j = succ $ mod (fromIntegral k) i
    temp <- readArray p i
    readArray p j >>= writeArray p i
    writeArray p j temp
  pe <- getElems p
  return (pe, t)

randomShuffle :: [a] -> [Int32] -> ([a], [Int32])
randomShuffle a rnds = (elems $ array (1, fromIntegral n) $ zip p a, t)
  where
    !n = length a
    (p, t) = randomPermutation rnds n
