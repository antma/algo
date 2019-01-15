{-# LANGUAGE FlexibleContexts #-}
module FenwickTree (
  build, update, reduce
) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Bits

build f zero n = do
  a <- newArray (0, n - 1) zero :: ST s (STUArray s Int Int)
  return (n, f, a)

update (n, f, a) x v = do
  let loop i = when (i < n) $ do
                 w <- readArray a i
                 writeArray a i (f w v)
                 loop (i .|. (succ i))
  loop x

--reduce on [0, x + 1)
reduce (n, f, a) zero x = do
  let loop i r = if i < 0 then return r
                 else do
                   w <- readArray a i
                   loop (pred (i .&. (succ i))) (f r w)
  loop x zero
