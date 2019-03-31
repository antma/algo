{-# LANGUAGE BangPatterns, FlexibleContexts, Safe #-}
module FenwickTree (
  update, reduce
) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST.Safe
import Data.Bits
import Data.Int

{------------------ Fenwick tree --------------------------------}
type FenwickTree a e = a Int e

update :: (MArray a e m) => FenwickTree a e -> (e -> e) -> Int -> m ()
{-# SPECIALIZE update :: FenwickTree (STUArray s) Int32 -> (Int32 -> Int32) -> Int -> ST s () #-}
update a f !x = do
  (0, !n) <- getBounds a
  let loop !i = when (i <= n) $ readArray a i >>= (writeArray a i . f) >> loop (i .|. succ i)
  loop x

--reduce on [0, x + 1)
reduce :: (MArray a e m) => FenwickTree a e -> (b -> e -> b) -> b -> Int -> m b
{-# SPECIALIZE reduce :: FenwickTree (STUArray s) Int32 -> (Int32 -> Int32 -> Int32) -> Int32 -> Int -> ST s Int32  #-}
reduce a f !zero !x =
  let loop !i !r = if i < 0 then return r
                   else readArray a i >>= loop (pred $! i .&. succ i) . f r
  in loop x zero
{---------------------------------------------------------------}
