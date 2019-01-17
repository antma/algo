{-# LANGUAGE FlexibleContexts #-}
module Primes (
  primeArray,
  isprime,
  primes
) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits

primeArray :: Int -> UArray Int Bool
primeArray n = runSTUArray $ do
  let m = max 1 (shiftR n 1)
      dhm :: Double
      dhm = sqrt $ fromIntegral $ pred n
      hm = ceiling dhm
  a <- newArray (0, m) False :: ST s (STUArray s Int Bool)
  writeArray a 0 True
  forM_ [1 .. pred hm] $ \p -> do
    l <- readArray a p
    when (not l) $ do
      let k = succ $ 2 * p
          loop j = when (j < m) $ do
                     writeArray a j True
                     loop (j + k)
      loop (2 * p * (succ p))
  return a

isprime p x = if even x then x == 2 else not (p ! (shiftR x 1))

primes n = 2 : (filter ( not . (pa ! ) . (`shiftR` 1))) [3, 5 .. n]
  where
    pa = primeArray n
