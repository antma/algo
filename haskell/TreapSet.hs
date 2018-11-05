{-# LANGUAGE BangPatterns #-}
module TreapSet (
  TreapSet,
  size, split, insert, merge, remove, count
) where

type Key = Int
data TreapSet = Node !Key !Int !Int TreapSet TreapSet | Nil

size Nil = 0
size (Node _ _ sz _ _) = sz

relax x y left right = Node x y (1 + (size left) + (size right)) left right 

split :: Key -> TreapSet -> (TreapSet, TreapSet)
split _ Nil = (Nil, Nil)

split key (Node x y _ left right)
  | key < x = let (l', r') = split key left in (l', relax x y r' right)
  | otherwise = let (l', r') = split key right in (relax x y left l', r')

insert :: Key -> Int -> TreapSet -> TreapSet
insert x y Nil = Node x y 1 Nil Nil 
insert x y t@(Node x' y' sz' left' right')
  | y' >= y = if x < x' then Node x' y' (1 + sz') (insert x y left') right'
                        else Node x' y' (1 + sz') left' (insert x y right')
  | otherwise = let (l'', r'') = split x t in relax x y l'' r''
  
merge Nil r = r
merge l Nil = l
merge l@(Node x' y' sz' l' r') r@(Node x'' y'' sz'' l'' r'')
  | y' > y'' = relax x' y' l' (merge r' r)
  | otherwise = relax x'' y'' (merge l l'') r''

remove _ Nil = Nil
remove x t@(Node x' y' sz' l' r')
  | x == x' = merge l' r'
  | x < x' = relax x' y' (remove x l') r'
  | otherwise = relax x' y' l' (remove x r')

count :: Int -> TreapSet -> Int
count x = count' 0
  where
    count' c Nil = c
    count' c (Node x' _ _ l' r')
      | x' < x = count' (c + 1 + (size l')) r'
      | otherwise = count' c l'
    
