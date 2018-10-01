
gcdExt :: Integral a => a -> a -> (a, a, a)
gcdExt a 0 = (a, 1, 0)
gcdExt a b = (g, x, y - x * c)
  where
    (c, d) = divMod a b
    (g, y, x) = gcdExt b d
