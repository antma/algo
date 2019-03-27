{-# LANGUAGE BangPatterns, FlexibleContexts, Safe #-}
module FenwickTree (
  build, update, reduce
) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST.Safe
import Data.Bits
import Data.Int

{------------------ Fenwick tree --------------------------------}
type FenwickTree a e = (Int, (e -> e -> e), a Int e)
build :: (MArray a e m) => (e -> e -> e) -> a Int e -> m (FenwickTree a e)
{-# SPECIALIZE build :: (Int32 -> Int32 -> Int32) -> STUArray s Int Int32 -> ST s (FenwickTree (STUArray s) Int32) #-}
build f a = do
  (0, !n) <- getBounds a
  return (n + 1, f, a)

update :: (MArray a e m) => FenwickTree a e -> Int -> e -> m ()
{-# SPECIALIZE update :: FenwickTree (STUArray s) Int32 -> Int -> Int32 -> ST s ()  #-}
update (n, f, a) !x !v = do
  let loop i = when (i < n) $ do
                 w <- readArray a i
                 writeArray a i (f w v)
                 loop (i .|. (succ i))
  loop x

--reduce on [0, x + 1)
reduce :: (MArray a e m) => FenwickTree a e -> e -> Int -> m e
{-# SPECIALIZE reduce :: FenwickTree (STUArray s) Int32 -> Int32 -> Int -> ST s Int32  #-}
reduce (n, f, a) !zero !x = do
  let loop i r = if i < 0 then return r
                 else do
                   w <- readArray a i
                   loop (pred (i .&. (succ i))) (f r w)
  loop x zero
{---------------------------------------------------------------}
