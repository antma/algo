module Random (
  pmGen, randoms
) where

import Data.Int

pmGen :: Int32 -> Int32
pmGen x = fromIntegral $! mod ((16807::Int64) * fromIntegral x) 2147483647

randoms :: Int32 -> [Int32]
randoms = iterate pmGen
