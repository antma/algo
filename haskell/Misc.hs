module Misc (
  tuple2, tuple3
) where

tuple2 [] = []
tuple2 (x:y:l) = (x, y) : (tuple2 l)

tuple3 [] = []
tuple3 (x:y:z:l) = (x, y, z) : (tuple3 l)

tuple4 [] = []
tuple4 (w:x:y:z:l) = (w, x, y, z) : (tuple4 l)
