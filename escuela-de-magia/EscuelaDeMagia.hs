module EscuelaDeMagia
  (EscuelaDeMagia, fundarEscuela, estaVacia , registrar , magos , hechizosDe,leFaltanAprender,egresarUno,enseñar)
where 

    


data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)

{- INV.REP: 
     -- Los magos que se encuentran en el map deben estar en la pq de los magos y viceversa
     -- El nombre del mago del map debe ser el mismo que el nombre que tiene como clave
     -- Los hechizos que estan aprendidos por los magos en la pq, deben exisitr en el set de hechizozs de la escuela 
-}


fundarEscuela :: EscuelaDeMagia
estaVacia :: EscuelaDeMagia -> Bool
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
magos :: EscuelaDeMagia -> [Nombre]
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia


registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
-- creaM costo O(1)
-- lookupM costo O(log M) siendo M la cantidad de magos
-- assocM costo O(log M) siendo M la cantidad de magos
-- Por ende el costo de la funcion  es de O(log M) 
registrar  n (EDM s mm p) = let mago = creaM n in 
                            case lookupM n mm of 
                            Just m -> (EDM s mm p)
                            Nothing -> EDM s (assocM n mago mm) p  

magos :: EscuelaDeMagia -> [Nombre]
-- Tiene costo O(log M) ya que es el costo de domM O(log M)
magos (EDM s mm p) = domM mm 


hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
-- Precond: Existe el mago con el nombre dado.
-- Costo O(log M) ya que es el costo de lookupM donde M es la cantidad de magos en el map
hechizosDe n (EDM s mm p) = let mago = fromJust(lookupM n mm) in 
                              hechizos mago 

leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender n (EDM s mm p) = let h = hechizosDe n (EDM s mm p) in 
                                (sizeS s) - (sizeS h) 

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM s mm p) = let mago = maxPQ P in 
                          case lookupM (nombre mago) mm of 
                          Just m' -> (m', (EDM s (deleteM (nombre mago) mm) deleteMaxPQ p)) 
                        
                          
enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
-- costo lookupM y assocM O(log M) siendo M la cantidad de magos
-- costo aprender y addS (O log H) siendo H la cantidad de hechizos
-- Costo de enseñar (log H + M log M) siento m la cantidad de magos y h de hechizos
enseñar h n (EDM s mm p) = case lookupM n mm of 
                        Nothing -> "debe exisitir el alumno"
                        Just m' -> let newH = aprender h m' in  
                             EDM (addS h s) (assocM n m' mm) (modificarPQ m' p) 



modificarPQ :: Mago -> PriorityQueue Mago -> PriorityQueue Mago
-- costo maxPQ O(1) 
-- costo de insertPQ y deleteMaxPQ O(log M) siendo H la cantidad de hechizos 
-- Costo de modificarPQ O(M log M) en peor caso ya que hace una funcion de costo O(log H) por cada elem del pq
modificarPQ m pq = let mago = maxPQ pq in 
                    if (m == mago) 
                    then insertPQ m (deleteMaxPQ pq)
                    else insertPQ mago (modificarPQ m (deleteMaxPQ pq))

