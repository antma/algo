{-# LANGUAGE FlexibleContexts #-}
module StringFunctions (
  AString,
  toArrayString, prefixFunction, match
) where

import qualified Data.ByteString.Char8 as C
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

type AString = UArray Int Char

toArrayString :: C.ByteString -> AString
toArrayString s = listArray (1, l) $ C.unpack s
  where
    l = C.length s

prefixFunction :: AString -> UArray Int Int
prefixFunction p = runSTUArray $ do
  let (_, m) = bounds p
  pi <- newArray_ (1, m) :: ST s (STUArray s Int Int)
  writeArray pi 1 0
  let loop q k = when (q <= m) $ do
          let c = p ! q
              loop' i =
                if i > 0 && (p ! (succ i)) /= c then readArray pi i >>= loop'
                else return i
          k' <- loop' k
          let k'' = if p ! (succ k') == c then succ k' else k'
          writeArray pi q k''
          loop (succ q) k''
  loop 2 0
  return pi

match :: UArray Int Int -> AString -> C.ByteString -> Bool
match pi p s = match' s 0
  where
    (_, m) = bounds p
    match' s q 
      | C.null s = False
      | otherwise = if q'' == m then True else match' (C.tail s) q''
        where
          c = C.head s
          q' = head $ dropWhile (\x -> x > 0 && (p ! (succ x)) /= c) $ iterate (pi !) q
          q'' = if p ! (succ q') == c then succ q' else q'
