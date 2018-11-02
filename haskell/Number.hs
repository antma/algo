module Number (
  gcdExt,
  SieveArray,
  sieveArray, primeFactorizationArray, totientArray, factors
) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Data.Array.ST
import Data.Array.MArray as M

gcdExt :: Integral a => a -> a -> (a, a, a)
gcdExt a 0 = (a, 1, 0)
gcdExt a b = (g, x, y - x * c)
  where
    (c, d) = divMod a b
    (g, y, x) = gcdExt b d

type SieveArray = UA.UArray Int Int
sieveArray :: Int -> SieveArray

sieveArray n = runSTUArray $ do
  a <- newListArray (0, n) [if odd x then x else 2 | x <- [0 .. n]]
  forM_ (takeWhile (\p -> p * p <= n) [3, 5 ..]) $ \p -> do
    l <- readArray a p
    if l == p then do
      forM_ (takeWhile ( <= n) [p * p, (p + 2) * p ..]) $ \o -> do
        k <- readArray a o
        if k == o then writeArray a o p else return ()
    else return ()
  return a

primeFactorizationArray :: SieveArray -> SieveArray

primeFactorizationArray p = runSTUArray $ do
  let (_, n) = UA.bounds p
  c <- M.newArray_ (0, 2 * n + 1)
  forM_ [2 .. n] $ \i -> do
    let k = p UA.! i
        j = i `div` k
        i2 = 2 * i
        j2 = 2 * j
    if (p UA.! j) == k then do
      cj <- readArray c j2
      writeArray c i2 (succ cj)
      nj <- readArray c (succ j2)
      writeArray c (succ i2) nj
    else do
      writeArray c i2 1
      writeArray c (succ i2) j
  return c

totientArray :: SieveArray -> SieveArray -> SieveArray

totientArray p pf = runSTUArray $ do
  let (_, n) = UA.bounds p
  phi <- M.newArray_ (0, n)
  writeArray phi 0 0
  writeArray phi 1 1
  forM_ [2 .. n] $ \i -> do
    let x = p UA.! i
        j = pf UA.! (succ (2 * i))
        f = (i `div` j) `div` x
    pj <- readArray phi j
    writeArray phi i ( (pred x) * f * pj)
  return phi

factors _ _ 1 = []
factors sa pfa n = (sa UA.! n, pfa UA.! i2) : (factors sa pfa $ pfa UA.! (succ i2))
  where
    i2 = 2 * n

