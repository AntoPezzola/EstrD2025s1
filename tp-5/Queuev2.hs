module QueueV2
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
enqueue a (Q qs) = a : qs
firstQ (Q qs) = elPrimeroDeQ qs 
dequeue (Q qs) = sinElPrimeroDeQ qs

elPrimeroDeQ :: [a] -> a
elPrimeroDeQ (x:xs) = if null xs 
                      then x 
                      else elPrimeroDeQ xs 

sinElPrimeroDeQ :: [a] -> [a]
sinElPrimeroDeQ (x:xs) = if null xs 
                         then sinElPrimeroDeQ xs 
                         else sinElPrimeroDeQ x:xs






