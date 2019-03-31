{-# LANGUAGE BangPatterns #-}
module TreapMap (
  TreapMap(..),
  sizem, tmInsert, tmIncrement, tmCount
) where

import Data.Int

type Key = Int32
type Value = Int32
type RandomValue  = Int32
data TreapMap = NodeM {-# UNPACK #-} !Key {-# UNPACK #-} !Value {-# UNPACK #-} !RandomValue {-# UNPACK #-} !Int32 TreapMap TreapMap | NilM

_relaxm :: Key -> Value -> RandomValue -> TreapMap -> TreapMap -> TreapMap
_relaxm !x !v !y l@(NodeM _ _ _ ls _ _) r@(NodeM _ _ _ rs _ _) = NodeM x v y (v + ls + rs) l r
_relaxm !x !v !y l@(NodeM _ _ _ sz _ _) NilM = NodeM x v y (v + sz) l NilM
_relaxm !x !v !y NilM r@(NodeM _ _ _ sz _ _) = NodeM x v y (v + sz) NilM r
_relaxm !x !v !y NilM NilM = NodeM x v y v NilM NilM

sizem :: TreapMap -> Int32
sizem NilM = 0
sizem (NodeM _ _ _ sz _ _) = sz

splitm :: Key -> TreapMap -> (TreapMap, TreapMap)
splitm _ NilM = (NilM, NilM)

splitm !key (NodeM x v y _ left right)
  | key < x = let (l', r') = splitm key left in (l', _relaxm x v y r' right)
  | otherwise = let (l', r') = splitm key right in (_relaxm x v y left l', r')

tmInsert :: Key -> RandomValue -> TreapMap -> TreapMap
tmInsert !x !y NilM = NodeM x 1 y 1 NilM NilM
tmInsert !x !y t@(NodeM x' v' y' sz' left' right')
  | y' >= y = if x < x' then NodeM x' v' y' (succ sz') (tmInsert x y left') right'
                        else NodeM x' v' y' (succ sz') left' (tmInsert x y right')
  | otherwise = let (l'', r'') = splitm x t in _relaxm x 1 y l'' r''

tmIncrement :: Key -> TreapMap -> TreapMap
tmIncrement !x (NodeM x' v' y' sz' l' r') = case compare x x' of
  LT -> NodeM x' v' y' (succ sz') (tmIncrement x l') r'
  GT -> NodeM x' v' y' (succ sz') l' (tmIncrement x r')
  EQ -> NodeM x' (succ v') y' (succ sz') l' r'

tmCount :: Key -> TreapMap -> Int32
tmCount !x = count' 0
  where
    count' !c NilM = c
    count' !c (NodeM x' v' _ _ l' r')
      | x' < x = count' (c + v' + sizem l') r'
      | otherwise = count' c l'

tmHeight :: TreapMap -> Int
tmHeight NilM = 0
tmHeight (NodeM _ _ _ _ l r) = succ $ max (tmHeight l) (tmHeight r)
