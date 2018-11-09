{-# LANGUAGE FlexibleContexts #-}
module Sort (
  qsort, lowerbound
) where

import Text.Printf
import Data.Array
import Data.Array.ST
import Data.Bits
import Control.Monad.ST
import qualified Data.Array.Unboxed as U

qsort :: [Int] -> [Int]
qsort lst = U.elems a
  where
    ll = pred $ length lst
    a = runSTUArray $ do
      x <- newListArray (0, ll) lst :: ST s (STUArray s Int Int)
      let
        srt l r = do
          xm <- readArray x ((2 * l + r) `div` 3)
          let
            f i j
              | i > j = return (i, j)
              | otherwise = do
                   let g' i = do
                         xi <- readArray x i
                         if xm > xi then g' (succ i) else return i
                       g'' j = do
                         xj <- readArray x j
                         if xm < xj then g'' (pred j) else return j
                   i' <- g' i
                   j' <- g'' j
                   if i' <= j' then do
                     v1 <- readArray x i'
                     v2 <- readArray x j'
                     writeArray x i' v2
                     writeArray x j' v1
                     f (succ i') (pred j')
                   else f i' j'
          (i'', j'') <- f l r
          if l < j'' then srt l j'' else return ()
          if i'' < r then srt i'' r else return ()
          return ()
      srt 0 ll
      return x

lowerbound a x = loop (pred l') (succ h')
  where
    (l', h') = bounds a
    loop l r
      | r - l > 1 = if (a ! m) < x then loop m r else loop l m
      | otherwise = l
      where
        m = (l + r) `shiftR` 1
