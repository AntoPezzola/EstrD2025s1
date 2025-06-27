tripulantes :: Nave -> Set Tripulante
--Prop: Denota los tripulantes de la nave
tripulantes n = tripulantes' (sectores n) n 

tripulantes' :: [Sector] -> Nave -> Set Tripulante
-- costo de tripulantesDe O(log S) siendo S la cantidad de sectores.
-- costo de union O(P log P) siendo P la cantidad de tripulantes de la nave. 
-- costo O(S * (log S + P log P))
tripulantes' [] n = emptyS
tripulantes' (s:ss) n = 
                    let trip = tripulantesDe s n in 
                     union trip (tripulantes' ss n)