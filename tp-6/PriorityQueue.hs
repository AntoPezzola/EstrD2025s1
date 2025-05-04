module PriorityQueue
  (PriorityQueue, emptyPQ, isEmptyPQ , insertPQ , findMinPQ , deleteMinPQ)
where 


data PriorityQueue = PQ [a]


emptyPQ :: PriorityQueue a
isEmptyPQ :: PriorityQueue a -> Bool
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
findMinPQ :: Ord a => PriorityQueue a -> a
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a


emptyPQ              = PQ []  -- O(1)
isEmptyPQ (PQ pqs)   = null pqs  -- O(1)
insertPQ x (PQ pqs)  = PQ (x:pqs) -- O(1)
findMinPQ (PQ pqs)   = minimum pqs -- O(n) 
deleteMinPQ (PQ pqs) = borrarMin pqs  -- O(n)


borrarMin :: Ord a => [a] -> [a]
-- PRECOND : La lista no es vacia
borrarMin xs = borrar (minimum xs) xs


borrar :: Ord a => a -> [a] -> [a]
borrar _ [] = error "La lista no debe ser vacía"
borrar n (x:xs) = if (n == x) 
                  then xs 
                  else x : borrar n xs 

