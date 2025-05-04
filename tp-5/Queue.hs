module Queue
  (Queue, emptyQ, isEmptyQ , enqueue , firstQ , dequeue)
where 

data Queue = Q [a]

emptyQ :: Queue a 
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
firstQ :: Queue a -> a
dequeue :: Queue a -> Queue a


emptyQ = Q []
isEmptyQ (Q qs) = null qs
enqueue a (Q qs) = agregarAlFinal qs a
firstQ (Q qs) = elPrimero qs 
dequeue (Q qs) = sinElPrimero qs 

elPrimero :: [a] -> a
elPrimero [] = []
elPrimero (x:xs) = x 

sinElPrimero :: [a] -> [a] 
sinElPrimero [] = []
sinElPrimero (x:xs) = xs 

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] n = [n]
agregarAlFinal (x:xs) n = x : agregarAlFinal xs n 





