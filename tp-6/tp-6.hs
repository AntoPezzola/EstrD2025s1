import Map

ejM = assocM "FF"       53
    $ assocM "Ale"      36
    $ assocM "Cristian" 29
    $ emptyM 

ejA = assocM "Anto"     24
    $ assocM "Cristian" 30
    $ emptyM 

-- PREGUNTAR
valuesM :: Eq k => Map k v -> [Maybe v]
-- Costo O(n'2) ya que es el costo de valuesM', donde recorre la lista
valuesM mp = valuesM' mp (keys mp) 

valuesM' :: Eq k => Map k v -> [k] -> [Maybe v] 
-- Costo O(n'2) ya que hace una op de costo O(n) por cada elem de la lista
-- donde n es la log de la lista 
-- lookupM O(n) 
valuesM' mp []     = []
valuesM' mp (k:ks) =  lookupM k mp : valuesM' mp ks 

-- todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- --Prop: indica si en el map se encuentran todas las claves dadas
-- -- COSTO O(n'2) ya que hace una op pertenece de costo O(n) pertence y otra keys con costo O(n) 
-- -- por cada elem de la recu  
-- todasAsociadas [] _      = True
-- todasAsociadas (k:ks) mp = pertenece k (keys mp) && todasAsociadas ks mp 

-- pertenece :: Eq k => k -> [k] -> Bool
-- -- Costo O(n) una op constante por cada elem de mi lista en peor caso
-- pertenece x [] = False
-- pertenece x (n:ns) = x == n || pertenece x ns 

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Prop: indica si en el map se encuentran todas las claves dadas
-- COSTO O(n'2) ya que hace una op de costo O(n) por cada elem de mi lisra
todasAsociadas [] _      = True
todasAsociadas (k:ks) mp = existeEnMap k mp && todasAsociadas ks mp 

existeEnMap ::  Eq k => k -> Map k v -> Bool
-- Costo O(n) ya que usa lookupM el cual tiene ese costo
existeEnMap k mp = case lookupM k mp of 
                   Just v -> True
                   Nothing -> False 

listToMap :: Eq k => [(k, v)] -> Map k v
--costo de emptyM --> Costo O(1)
--costo de assocM --> Costo O(n) 
--costo de listToMap es de O(n'2) ya que hace una op de costo O(n) por cada elem de la recu
listToMap []     = emptyM
listToMap ((k,v):ks) = assocM k v (listToMap ks) 

mapToList :: Eq k => Map k v -> [(k, v)]
-- O(n'2) ya que es el costo de armarLista
mapToList mp = armarLista mp (keys mp)

armarLista :: Eq k => Map k v -> [k] -> [(k,v)]
-- costo lookupM O(n)
-- costo fromJust O(1)
-- costo de armarList O(n'2) ya que hace una op de costo O(n) por cada elem
-- de recu
armarLista mp []     = []
armarLista mp (k:ks) = (k,fromJust(lookupM k mp)) : armarLista mp ks

fromJust :: Maybe a -> a
  -- PRECOND: no puede ser Nothing
fromJust (Just x) = x  

agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- Prop: dada una lista de pares clave valor, agrupa los valores
-- de los pares que compartan la misma clave.
--------------------------------------------------------
-- costo lookupM y assocM O(n), el costo de agruparEq es de O(n'2)
-- ya que hace dos op de costo O(n) por cada elm de la lista
agruparEq []         = emptyM
agruparEq ((k,v):ks) = case lookupM k (agruparEq ks) of 
                       Just v' -> assocM k (v:v') (agruparEq ks)
                       Nothing -> assocM k (v:[]) (agruparEq ks)

incrementar :: Eq k => [k] -> Map k Integer -> Map k Integer
-- costo lookupM y assocM O(n), el costo de incrementar es de O(n'2)
-- ya que hace dos op de costo O(n) por cada elm de la lista de ks
incrementar [] mp = mp
incrementar (k:ks) mp = let mp' = incrementar ks mp in   
                        case lookupM k mp' of 
                       Just v' -> assocM k (v'+ 1) mp'
                       Nothing -> assocM k 1 mp'

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
-- PROP: : dado dos maps se agregan las claves y valores del primer map
--en el segundo. Si una clave del primero existe en el segundo, 
--es reemplazada por la del primero.
mergeMaps mp1 mp2 = mergeMapK mp1 (keys mp1) mp2 


mergeMapK ::  Eq k => Map k v -> [k] -> Map k v -> Map k v
mergeMapK mp1 [] mp2     = mp2
mergeMapK mp1 (k:ks) mp2 = assocM k (fromJust (lookupM k mp1)) (mergeMapK mp1 ks mp2) 

                        