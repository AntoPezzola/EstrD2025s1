

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

-- INV.REP : * El sector en la tupla (Sector, Int) debe existir como clave en el map
--           * Todos los tripulantes del heap deben existir en los valores del map 
--           * Un tripulante debe aparecer solo una vez como valor en el set 



naveVacia :: [Sector] -> Nave
tripulantesDe :: Sector -> Nave -> Set Tripulante
sectores :: Nave -> [Sector]
conMayorRango :: Nave -> Tripulante
conMasTripulantes :: Nave -> Sector
conRango :: Rango -> Nave -> Set Tripulante
sectorDe :: Tripulante -> Nave -> Sector
agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
rango :: Tripulante -> Rango


naveVacia :: [Sector] -> Nave
naveVacia ss = MkN (losSectoresNuevos ss) emptyH ((head ss), 0)

losSectoresNuevos ::[Sector] -> Map Sector (Set Tripulante)
-- costo de assocM O(log S) siendo S la cantidad de sectores de la lista.
--  costo O(s log s) ya que por cada elem de la lista de s, hago una op de mismo costo
losSectoresNuevos []     = emptyM
losSectoresNuevos (s:ss) = assocM s emptyS (losSectoresNuevos ss)


tripulantesDe :: Sector -> Nave -> Set Tripulante
-- Prop: Obtiene los tripulantes de un sector.
-- costo de assocM O(log S) siendo S la cantidad de sectores de la lista.
tripulantesDe s (MkN mp _ _) = case lookupM s mp of 
                                    Just setTrip -> setTrip
                                    Nothing -> error "No existe ese sector en la nave" 

sectores :: Nave -> [Sector]
-- Prop: Denota los sectores de la nave
-- Costo: O(S) siendo S la cantidad de sectores.
sectores (MkN mp _ _) = domM mp 

conMayorRango :: Nave -> Tripulante
-- Costo O(1) ya que es el costo de findMin 
conMayorRango (MkN _ hep _) = findMin hep

conRango :: Rango -> Nave -> Set Tripulante
-- costo findMin O(1)
-- costo addS O(log P) siendo P la cantidad de tripulantes
-- costo O(P log P) ya que en peor caso recorro todo el heap y hago una op de costo O(log p) por cada elem del heap
conRango r (MkN m heap tupla) =  
                              if (isEmptyH heap) 
                              then emptyS
                              else let trip = findMin heap in 
                              if (rango (trip) == r)
                              then addS trip (conRango r (MkN m (deleteMin heap) tupla))
                              else conRango r (MkN m heap tupla)


---- PREGUNTAR SI NO ES S * PORQUE ES EL MISMO TAMAÑO
sectorDe :: Tripulante -> Nave -> Sector
sectorDe t (MkN m heap tupla) = sectorDe' (domM m) t m  

sectorDe' :: [Sector] -> Tripulante -> Map Sector (Set Tripulante) -> Sector
-- Costo de O(log S) siendo S la cantidad de sectores de la nave
-- costo belongs O(log P) siendo P la cantidad de tripulantes
-- costo O(S log S + log P) ya que hago por cada elm de la recu en peor caso de O(log s + log p)  -> (s * log s + log p) -> (s log s + log p)

sectorDe' (s:ss) t mp = let setTrip = fromJust(lookupM s mp) in 
                        if (belongs t setTrip)
                        then s 
                        else sectorDe' ss t mp 


agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
-- costo de agregarAlMap O(log S + log P)
-- costo de lookupM O(log S) siendo S la cantidad de sectores de la nave 
-- costo de sizeS O(1)
-- costo de insertH O(log P) siendo P la cantidad de tripulantes de la nave
agregarTripulante t s (MkN m heap (s,n)) =  let sectorNew = agregarAlMap t s m 
                                                setTrip = fromJust(lookupM s sectorNew) in 
                                            if ((sizeS setTrip) > n)
                                            then  MkN sectorNew (insertH t heap) (sectorNew, (sizeS setTrip))
                                            else  MkN sectorNew (insertH t heap) (s,n)


agregarAlMap :: Tripulante -> Sector -> Map Sector (Set Tripulante) ->  Map Sector (Set Tripulante)
-- costo de lookupM y assocM O(log S) siendo S la cantidad de sectores de la nave 
-- costo de addS O(log P) siento P la cantidad de tripulantes de la nave
-- costo de agregarAlMap O(log S + log P)
agregarAlMap t s mp = case lookupM s mp of 
                      Just setTrip -> assocM s (addS t setTrip) mp
                      Nothing -> error "no existe ese sector en el map"

