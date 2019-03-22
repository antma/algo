{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Primes (
  primeArray,
  isprime,
  primes,
  factors, factorization,
  divisors,
  eulerTotientFunction
) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.List

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
    unless l $ do
      let k = succ $ 2 * p
          loop j = when (j < m) $ do
                     writeArray a j True
                     loop (j + k)
      loop (2 * p * succ p)
  return a

isprime :: UArray Int Bool -> Int -> Bool
isprime p x = if even x then x == 2 else not (p ! shiftR x 1)

primes :: Int -> [Int]
primes n = 2 : filter ( not . (pa ! ) . (`shiftR` 1)) [3, 5 .. n]
  where
    pa = primeArray n

factors :: [Int] -> Int -> [Int]
factors p n = loop n p []
  where
    loop 1 _ r = r
    loop m l@(x:xs) r
      | x * x > m = m : r
      | mod m x == 0 = loop (div m x) l (x:r)
      | otherwise = loop m xs r

factorization :: [Int] -> Int -> [(Int, Int)]
factorization primes' n = map (\l -> (head l, length l)) $ group $ factors primes' n

divisors :: [Int] -> Int -> [Int]
divisors primes' n = sort $ loop (factorization primes' n) 1 []
  where
    loop [] !m r = m : r
    loop ((!c, !p):xs) m r = snd $ (iterate f (m, r)) !! (p + 1)
      where
        f (!m', r') = (m' * c, loop xs m' r')

eulerTotientFunction :: [Int] -> Int -> Int
eulerTotientFunction primes' n = product $ map (\(!a, !c) -> let !q = a ^ (pred c) in q * pred a) $ factorization primes' n
