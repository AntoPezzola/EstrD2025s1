

data Torneo =   ConsT (Map Nombre Equipo)   ----- relaciona los equipos con su nombre 
                      (Map Nombre Nombre)   ----- relaciona los nombres de jugadores (las claves) con nombre de equipos (los valores)
                      (PriorityQueue Equipo) ---- Los equipos ordenados por prioridad en base a sus goles

{- INV.REP: * Los nombres de los equipos que estan en el primer map, deben existir como valor en el segundo map
            * Los equipos que estan en la pq deben existir en el primer map como claves, y viciversa 
-}


equipo :: Nombre -> Torneo -> Maybe Equipo
-- Propósito: dado un nombre de equipo devuelve al equipo con dicho nombre.
-- Eficiencia: O(log N )
equipo n (ConsT mp _ _ ) = lookupM n mp 


jugadores :: Torneo -> [Nombre]
-- Propósito: denota la lista de jugadores del torneo.
-- Eficiencia: O(N )
jugadores (ConsT _ mp _ ) =  domM mp 


registrarGol :: Nombre -> Nombre -> Torneo -> Torneo
-- Propósito: dados un nombre de jugador y un nombre de equipo, ingresa un gol anotado por el jugador dado para el equipo
-- dado.
-- Precondición: existe un jugador y un equipo con dichos nombres.
-- Eficiencia: O(N log N )
registrarGol nJ nE (ConsT mpE mpJ pq) = let equipo = fromJust(lookupM nE mpE) 
                                            eUpdate = anotarGol nJ equipo in 
                                        ConsT (assocM nE equipo mpE) mpJ (reemplazarPq eUpdate pq)

reemplazarPq :: Equipo -> PriorityQueue Equipo -> PriorityQueue Equipo
-- costo maxPQ O(1)
-- costo de insertPQ y deleteMaxPQ O(log P)
-- en peor caso recorro toda la pq y hago por cada equipo las op de insertPQ y deleteMaxPQ O(log P)
-- -> el costo de reemplazar pq es O(P log P)
reemplazarPq e pq =     
                      if (isEmptyPQ pq)
                      then emptyPQ
                      else 
                        let eMax = maxPQ pq 
                            delPq = deleteMaxPQ pq 
                        in (if nombre eMax == nombre e )
                           then insertPQ e (reemplazarPq e delPq)
                           else insertPQ eMax (reemplazarPq e delPq)


ingresarJugador :: Nombre -> Nombre -> Torneo -> Torneo
-- Propósito: dado un nombre de jugador y un nombre de equipo, ingresa al torneo dicho jugador, con cero goles, agregándolo
-- al equipo dado.
-- Eficiencia: O(N log N )
-- Costo de lookupM y assocM O(log N)
-- Costo de insert 
ingresarJugador nJ nE (ConsT mpE mpJ pq) =  case lookupM nE mpE of 
                                           Just e -> 
                                                let eUpdate = fichar nJ e in 
                                                ConsT  (assocM nE eUpdate mpE) (assocM nJ nE mpJ) pq 
                                            Nothing -> 
                                                    let newE = fundar nE 
                                                        newEUpdate = fichar nJ newE in 
                                                ConsT (assocM nE newEUpdate mpE) (assocM nJ nE mpJ) (insertPQ newEUpdate pq) 