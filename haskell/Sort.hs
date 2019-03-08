{-# LANGUAGE Safe, FlexibleContexts #-}
module Sort (
  qsort, lowerbound
) where

import Data.Array
import Data.Array.ST.Safe
import Data.Bits
import Control.Monad
import Control.Monad.ST
import qualified Data.Array.Unboxed as U

qsort :: (Ord e, MArray a e m) => a Int e -> [Int] -> m ()
qsort x rnds = do
  bds@(l',r') <- getBounds x
  let
    srt l r (y:ys) = do
      xm <- readArray x (l + mod y (r - l + 1))
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
               else return (i', j')
      (i'', j'') <- f l r
      when (l < j'') $ srt l j'' ys
      when (i'' < r) $ srt i'' r ys
  srt l' r' rnds

lowerbound a x = loop (pred l') (succ h')
  where
    (l', h') = bounds a
    loop l r
      | r - l > 1 = if (a ! m) < x then loop m r else loop l m
      | otherwise = l
      where
        m = (l + r) `shiftR` 1
