{-# LANGUAGE BangPatterns, FlexibleContexts, Safe #-}
{-# Options_GHC -O2 #-}
module Heap (
  heapInit,
  heapFromList,
  heapExtractMin,
  heapDecreaseKey,
  heapUpdateKey,
  heapNull
) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST.Safe
import Data.Bits
import Data.List

{----------------------------------- Heap -------------------------------------}
type HeapEntry = Int
type Heap s = (STUArray s Int HeapEntry, STUArray s Int Int, STUArray s Int Int)
heapInit :: Int -> HeapEntry -> ST s (Heap s)
heapInit n default_value = do
  d <- newArray (1, n) default_value :: ST s (STUArray s Int HeapEntry)
  h <- newArray_ (0, n) :: ST s (STUArray s Int Int)
  g <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
  writeArray h 0 0
  return (d, h, g)

heapifyFront :: Heap s -> Int -> ST s ()
heapifyFront (d, h, g) !k = do
  let get !i = do
      hi <- readArray h i
      dhi <- readArray d hi
      return (i, hi, dhi)
  sz <- readArray h 0
  (_, he, dhe) <- get k
  let
    loop !i = do
    let !j = shiftL i 1
    if j <= sz then do
      (l, hl, dhl) <- do
        if j >= sz then get j
        else do
          a'@(_, _, !dhj') <- get j
          a''@(_, _, !dhj'') <- get $ succ j
          return $ if dhj'' < dhj' then a'' else a'
      if dhl < dhe then do
        writeArray h i hl
        writeArray g hl i
        loop l
      else return i
    else return i
  i' <- loop k
  when (i' /= k) $ do
    writeArray h i' he
    writeArray g he i'

heapFromList :: [HeapEntry] -> ST s (Heap s)
heapFromList l = do
  let !n = length l
      !m = shiftR n 1
  d <- newListArray (1, n) l :: ST s (STUArray s Int HeapEntry)
  h <- newListArray (0, n) (n : [1 .. n]) :: ST s (STUArray s Int Int)
  g <- newListArray (1, n) [1 .. n] :: ST s (STUArray s Int Int)
  let hp = (d, h, g)
  mapM_ (heapifyFront hp) [ m, pred m .. 1]
  return hp

heapifyBack :: Heap s -> Int -> ST s ()
heapifyBack (d, h, g) !k = do
  let get !i = do
      hi <- readArray h i
      dhi <- readArray d hi
      return (hi, dhi)
  (!hk, !dhk) <- get k
  let
    loop !i = if i > 1 then do
                let !j = shiftR i 1
                (!hj, !dhj) <- get j
                if dhk < dhj then do
                  writeArray h i hj
                  writeArray g hj i
                  loop j
                else return i
              else return 1
  i' <- loop k
  when (i' /= k) $ do
    writeArray h i' hk
    writeArray g hk i'

heapExtractMin :: Heap s -> ST s (Int)
heapExtractMin hp@(d, h, g) = do
  sz <- readArray h 0
  writeArray h 0 (pred sz)
  if sz > 0 then do
    he <- readArray h 1
    writeArray g he 0
    when (sz > 1) $ do
      h1 <- readArray h sz
      writeArray h 1 h1
      writeArray g h1 1
      heapifyFront hp 1
    return he
  else error "extract min from empty heap"

heapDecreaseKey :: Heap s -> Int -> HeapEntry -> ST s ()
heapDecreaseKey hp@(d, h, g) !k !val = do
  dk <- readArray d k
  if dk > val then do
    pos <- readArray g k
    writeArray d k val
    if pos == 0 then do
      sz <- readArray h 0
      let ssz = succ sz
      writeArray h 0 ssz
      writeArray h ssz k
      writeArray g k ssz
      heapifyBack hp ssz
    else heapifyBack hp pos
  else return ()

heapUpdateKey :: Heap s -> Int -> HeapEntry -> ST s ()
heapUpdateKey hp@(d, h, g) !k !val = do
  pos <- readArray g k
  dk <- readArray d k
  if dk > val then do
    writeArray d k val
    heapifyBack hp pos
  else if dk < val then do
    writeArray d k val
    heapifyFront hp pos
  else return ()

heapNull :: Heap s -> ST s (Bool)
heapNull (_, h, _) = do
  sz <- readArray h 0
  return $! sz == 0

heapGetMin :: Heap s -> ST s (Int)
heapGetMin hp@(d, h, g) = readArray h 1 >>= readArray d
{--------------------------------------------------------------------------}
