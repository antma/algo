{-# LANGUAGE BangPatterns, FlexibleContexts, Safe #-}
{-# Options_GHC -O2 #-}
module SegmentTreeC (
  stBuild, stUpdate, stReduce
) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST.Safe
import Data.Bits
import Data.Int

{----------------------------------- Segment Tree (cummutative) -------------------------------------}
type SegmentTree s e = (Int, e -> e -> e, STUArray s Int e)

stBuild :: (MArray a e m) => (e -> e -> e) -> a Int e -> [e] -> m (Int, (e -> e -> e), a Int e)
{-# SPECIALIZE stBuild :: (Int32 -> Int32 -> Int32) -> STUArray s Int Int32 -> [Int32] -> ST s (SegmentTree s Int32) #-}
{-# SPECIALIZE stBuild :: (Int64 -> Int64 -> Int64) -> STUArray s Int Int64 -> [Int64] -> ST s (SegmentTree s Int64) #-}
stBuild f t a = do
  let !n = length a
      !n' = pred n
  forM_ (zip [n .. ] a) (uncurry $ writeArray t)
  forM_ [n', pred n' .. 0] $ \ !i -> let !k = shiftL i 1 in liftM2 f (readArray t k) (readArray t $! succ k) >>= writeArray t i
  return (n, f, t)

stUpdate :: (MArray a e m) => (Int, (e -> e -> e), a Int e) -> Int -> e -> m ()
{-# SPECIALIZE stUpdate :: SegmentTree s Int32 -> Int -> Int32 -> ST s () #-}
{-# SPECIALIZE stUpdate :: SegmentTree s Int64 -> Int -> Int64 -> ST s () #-}
stUpdate (n, f, t) !p !v = loop (p + n) v
  where
    loop !x !w = do
      writeArray t x w
      when (x > 1) $ liftM (f w) (readArray t $! xor 1 x) >>= loop (shiftR x 1)

stReduce :: (MArray a e m) => (Int, (e -> e -> e), a Int e) -> Int -> Int -> e -> m e
{-# SPECIALIZE stReduce :: SegmentTree s Int32 -> Int -> Int -> Int32 -> ST s Int32 #-}
{-# SPECIALIZE stReduce :: SegmentTree s Int64 -> Int -> Int -> Int64 -> ST s Int64 #-}
stReduce (n, f, t) !l !r !z = loop (l+n) (r+n) z
  where
    loop !l !r !z = if l < r then (if odd l then liftM (f z) (readArray t l) >>= loopr (succ l) r else loopr l r z) else return z
    loopr !l !r !z = if odd r then liftM (f z) (readArray t (pred r)) >>= loop (shiftR l 1) (shiftR (pred r) 1) else loop (shiftR l 1) (shiftR r 1) z

{-----------------------------------------------------------------------------------------------------}
