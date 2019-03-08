module Random (
  pmGen
) where

import Data.Int

pmGen :: Int -> Int
pmGen x = fromIntegral (mod ((16807::Int64) * fromIntegral x) 2147483647)
