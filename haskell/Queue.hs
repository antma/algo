module Queue (
  Queue,
  queueEmpty, queuePush, queuePop
) where

type Queue a = ([a], [a])
queueEmpty :: Queue a
queueEmpty = ([], [])
queuePush :: Queue a -> a -> Queue a
queuePush (l, r) x = (l, x : r)
queuePop :: Queue a -> (a, Queue a)
queuePop ( (x:xs), r) =  (x, (xs, r))
queuePop ( [], r)
  | null r = undefined
  | otherwise = queuePop (reverse r, [])
