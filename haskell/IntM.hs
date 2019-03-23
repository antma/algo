{-# LANGUAGE BangPatterns, Safe #-}
module IntM (
  add, sub, mul, pow, toMod
) where

import Data.Int
import Data.Bits

modulo = 1000000007 :: Int
modulo64 = fromIntegral modulo :: Int64

add :: Int -> Int -> Int
add !x !y = let !z = x + y in if z >= modulo then z - modulo else z

sub :: Int -> Int -> Int
sub !x !y = let !z = x - y in if z < 0 then z + modulo else z

mul :: Int -> Int -> Int
mul !x !y = fromIntegral $! (fromIntegral x * fromIntegral y) `mod` modulo64

pow :: Int -> Int -> Int
pow _ 0 = 1
pow x p = if odd p then mul x (mul e e) else mul e e
  where
    !e = pow x (p `shiftR` 1)

toMod :: Int64 -> Int
toMod x
  | z >= 0 = z
  | otherwise = z + modulo
  where
    !z = fromIntegral $! (x `mod` modulo64)
