{-# LANGUAGE BangPatterns #-}
module TreapSet (
  TreapSet(..),
  key, size, tsInsert, tsRemove, tsCount, tsKth
) where

import Data.Int
import Data.List

type Key = Int32
type RandomValue  = Int32
data TreapSet = Node {-# UNPACK #-} !Key {-# UNPACK #-} !RandomValue {-# UNPACK #-} !Int32 TreapSet TreapSet | Nil

key :: TreapSet -> Key
key (Node x _ _ _ _) = x

size :: TreapSet -> Int32
size Nil = 0
size (Node _ _ sz _ _) = sz

_relax :: Key -> RandomValue -> TreapSet -> TreapSet -> TreapSet
_relax !x !y l@(Node _ _ ls _ _) r@(Node _ _ rs _ _) = Node x y (succ $! ls + rs) l r
_relax !x !y l@(Node _ _ sz _ _) Nil = Node x y (succ sz) l Nil
_relax !x !y Nil r@(Node _ _ sz _ _) = Node x y (succ sz) Nil r
_relax !x !y Nil Nil = Node x y 1 Nil Nil

split :: Key -> TreapSet -> (TreapSet, TreapSet)
split _ Nil = (Nil, Nil)

split !key (Node x y _ left right)
  | key < x = let (l', r') = split key left in (l', _relax x y r' right)
  | otherwise = let (l', r') = split key right in (_relax x y left l', r')

tsInsert :: Key -> RandomValue -> TreapSet -> TreapSet
tsInsert !x !y Nil = Node x y 1 Nil Nil
tsInsert !x !y t@(Node x' y' sz' left' right')
  | y' >= y = if x < x' then Node x' y' (succ sz') (tsInsert x y left') right'
                        else Node x' y' (succ sz') left' (tsInsert x y right')
  | otherwise = let (l'', r'') = split x t in _relax x y l'' r''

merge :: TreapSet -> TreapSet -> TreapSet
merge Nil r = r
merge l Nil = l
merge l@(Node x' y' sz' l' r') r@(Node x'' y'' sz'' l'' r'')
  | y' > y'' = _relax x' y' l' (merge r' r)
  | otherwise = _relax x'' y'' (merge l l'') r''

tsRemove :: Key -> TreapSet -> TreapSet
tsRemove _ Nil = Nil
tsRemove !x (Node x' y' sz' l' r')
  | x < x' = _relax x' y' (tsRemove x l') r'
  | x > x' = _relax x' y' l' (tsRemove x r')
  | otherwise = merge l' r'

tsRemoveExistingKey :: Key -> TreapSet -> TreapSet
tsRemoveExistingKey !x (Node x' y' sz' l' r')
  | x < x' = Node x' y' (pred sz') (tsRemoveExistingKey x l') r'
  | x > x' = Node x' y' (pred sz') l' (tsRemoveExistingKey x r')
  | otherwise = merge l' r'

tsCount :: Key -> TreapSet -> Int32
tsCount !x = count' 0
  where
    count' !c Nil = c
    count' !c (Node x' _ _ l' r')
      | x' < x = count' (succ $! c + size l') r'
      | otherwise = count' c l'

tsKth :: TreapSet -> Int32 -> Key
tsKth t k
  | k < 0 || k >= size t = undefined
  | otherwise = key $ loop t k
  where
    loop cur@(Node x _ _ left right) k
      | k > ls = loop right (k - (ls + 1))
      | k < ls = loop left k
      | otherwise = cur
      where
        ls = size left

tsBuild :: [(Key, RandomValue)] -> TreapSet
tsBuild [] = Nil
tsBuild l = _relax x y (tsBuild tl) (tsBuild tr)
  where
    -- ((!x, !y), l') = foldr (\ e@(_, !y) (f@(_, !y'), l) -> if y > y' then (e, f : l) else (f, e : l)) (head l, []) $ tail l
    ((!x, !y), l') = foldl' (\ (f@(_, !y'), l) e@(_, !y) -> if y > y' then (e, f : l) else (f, e : l)) (head l, []) $ tail l
    (tl, tr) = partition ( ( < x) . fst) l'
