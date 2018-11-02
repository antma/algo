module Misc (
  tuple2
) where

tuple2 [] = []
tuple2 (x:y:l) = (x, y) : (tuple2 l)
