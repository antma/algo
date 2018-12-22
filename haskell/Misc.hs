module Misc (
  tuple2, tuple3, tuple4,
  ri, rl
) where

import qualified Data.ByteString.Char8 as C
import Data.Int

tuple2 [] = []
tuple2 (x:y:l) = (x, y) : (tuple2 l)

tuple3 [] = []
tuple3 (x:y:z:l) = (x, y, z) : (tuple3 l)

tuple4 [] = []
tuple4 (w:x:y:z:l) = (w, x, y, z) : (tuple4 l)

ri :: C.ByteString -> Int
ri s = r
  where Just (r, _) = C.readInt s

rl :: C.ByteString -> Int64
rl s = fromInteger r
  where Just (r, _) = C.readInteger s
