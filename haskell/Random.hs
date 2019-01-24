module Random (
  pmGen
) where

import Data.Int

pmGen :: Int -> Int
pmGen x = fromIntegral (mod z 2147483647)
  where y :: Int64
        y = fromIntegral x
        z = (16807 :: Int64) * y
