
data EmGame = AG (Map Personaje [Esmeralda]) (Map Esmeralda (Maybe Personaje)) (MaxHeap Personaje)
   
   {- Inv rep. Los personajes que existen en el maxHeap, no estan repetidos. 
               Los personajes que existen en la mapHeap, DEBEN existir en el primer map como claves P
               Los personajes que existen como claves en el primer map como P, con la lista de esmerarldas asociadas como valor, deben existir dentro del mxheap
               En el primer map, donde existe un peronsaje como clave, con una lista de esmeraldas como valor E, dentro de la lista no existen esmeraldas repetidas 
               Las esmeraldas que existen como valor en el primer map, asociadas a un personaje, DEBEN existir en el segundo map como clave esmeralda E
               Las esmeraldas que son clave en el segundo map el personaje que tienen asignado, DEBE exisitr como clave en el primer map, y la clave E, debe estar dentro
               de la lista de esmeraldas que tiene asociadas como valor 
   -}

iniciarJuego :: Set Personaje → Set Esmeralda → EmGame
-- dado un conjunto de personajes y un conjunto de esmeraldas (suponiendo ambos
-- no vacios) describe un juego inicial con esas esmeraldas y esos personajes sin
-- esmeraldas. Falla si alguno de los conjuntos esta vacio (O(P log P + E log E))
iniciarJuego sp se = let listS = set2list sp 
                         listE = set2list se 
                   in AG (armarMap listS) (armarMap listE) (armarHeap listS)

armarMap :: [Personaje] -> Map Personaje [Esmeralda]
armarMap [] = emptyM 
armarMap (p:ps) = assocM p [] (armarMap ps) 

armarMap :: [Esmeralda] -> Map Esmeralda (Maybe Personaje)
armarMap [] = emptyM 
armarMap (e:es) = assocM e Nothing (armarMap es) 

armarHeap ::  [Personaje] -> MaxHeap Personaje
armarHeap [] = emptyH
armarHeap (p:ps) = insertH p (armarHeap ps)


esmeraldasDe :: EmGame → Personaje → [Esmeralda]
-- dado 1 juego y un personaje,describe la lista de esmeraldas de ese personaje.
-- Falla si el personaje no esta en el juego (O(log P))
esmeraldasDe (AG pe _ _) p = case p pe of 
                            Just e -> e 
                            Nothing -> error "no existe el personaje en el juego"

 obtenerEsmeralda :: EmGame → Personaje → Esmeralda → EmGame
-- dado un juego,personaje y esmeralda dentro de ese juego,describe el resultado de
-- asignar la esmeralda al personaje suponiendo que esa esmeralda no esta
-- asignada. Falla si el personaje o la esmeralda no esta en el juego (O (log P + log
-- E))
obtenerEsmeralda (AG pe es maxP) p e = case lookupM e es of 
                                      Just p' -> case p' of 
                                              Nothing -> AG (asignarEsmeralda p e pe) (asignarPersonaje e p es) maxP
                                              Just personaje -> error "la esmeralda ya esta asignada a un personaje" 
                                      Nothing -> error "no existe la esmeralda en el juego"

asignarEsmeralda :: Personaje -> Esmeralda ->  Map Personaje [Esmeralda] -> Map Personaje [Esmeralda]
asignarEsmeralda p e mp = case lookupM p mp of 
                        Just es -> if (elem e es) 
                                then mp 
                                else assocM p e:es mp 
                        Nothing -> error "no existe ese personaje"

asignarPersonaje :: Esmeralda -> Personaje -> Map Esmeralda (Maybe Personaje) -> Map Esmeralda (Maybe Personaje)
asignarPersonaje e p mp = case e mp of 
                        Just p' -> case p' of 
                                   Nothing -> assocM e (Just p) mp 
                                   Just pe -> error "La esmeralda ya tiene un personaje asignado"

 ganarEsmeralda :: EmGame → Personaje → Personaje → Esmeralda -> EmGame
-- dado 1 juego,2 personajes y 1 esmeralda dentro de ese juego,describe el resultado
-- de realizar la competencia entre ambos personajes,suponiendo que esa esmeralda
-- la tiene alguno de los personajes,y se la queda el mas poderoso. Falla si los
-- personajes o la esmeralda no son parte del juego, y si la esmeralda no la tiene uno
-- de los 2 personajes (O(log P + E))
ganarEsmeralda (AG pe es maxP) p1 p2 e = if (poder p1 > poder p2)
                                         then AG (actualizarGanador pe p1 p2 es e) (actualizarEsmeraldaGanador es p1 p2 e) maxP
                                         else AG (actualizarGanador pe p2 p1 es e) (actualizarEsmeraldaGanador es p2 p1 e) maxP

actualizarGanador :: Map Personaje [Esmeralda] -> Personaje -> Personaje -> Esmeralda -> Map Personaje [Esmeralda]
actualizarGanador mp ganador perdedor e = case lookupM ganador mp of 
                                          Nothing -> error "no existe el jugador"
                                          Just es -> if (elem e es)
                                                     then mp 
                                                     else case lookupM perdedor mp of 
                                                          Just es' -> if (elem e es')
                                                                      then assocM perdedor (sinElElemento e es') ( assocM ganador (e:es) mp)
                                                                      else assocM ganador (e:es) mp 
                                                         Nothing -> error "no existe el jugador"

actualizarEsmeraldaGanador :: Map Personaje [Esmeralda] -> Personaje -> Personaje -> Esmeralda -> Map Personaje [Esmeralda]
actualizarEsmeraldaGanador mp ganador perdedor e = case lookupM e mp of 
                                                   Just p' -> case p' ->
                                                          Just per -> if(per == ganador) 
                                                                       then mp
                                                                       else if(p' == perdedor)  
                                                                            then assocM e (Just ganador) mp 
                                                                            else error "los personajes dados no tiene la esmeralda asignada"
                                                          Nothing -> error ".."
                                                   Nothing -> error ".."

                    
usarEsmeralda :: EmGame → Personaje → Esmeralda → EmGame
-- dado un juego,personaje,esmeralda (dentro de ese juego)describe el juego que
-- resulta de la ultilizacion de su esmeralda por el personaje,suponiendo que el
-- personaje tiene la esmeralda. Falla si el personaje o la esmeralda no son parte del
-- juego, y si la esmeralda no la tiene el personaje (O(log P + E))
usarEsmeralda (AG pe es maxP) p e = case lookupM p pe of 
                                    Nothing -> error "no existe el jugador"
                                    Just es -> if(elem e es)
                                               let pNew = aumentarPoderDe p ((poder p) * 2) in 
                                               then AG (actualizarMapJugador p pNew pe) (deleteM e es) (actualizarHeap pNew p)
                                               else error "el jugador no tiene la esmeralda"


                                            
actualizarMapJugador ::Personaje ->  Personaje -> Map Personaje [Esmeralda] -> Map Personaje [Esmeralda]
actualizarJugadorEsmeralda pOld pNew mp = case lookupM pOld mp of
                                    Just es -> assocM pNew es (deleteM pOld mp)
                                    Nothing -> error "no existe el personaje"

actualizarHeap :: Personaje -> Personaje -> MaxHeap Personaje -> MaxHeap Personaje
actualizarHeap pNew pOld mx = let pMax = findMax mx in 
                                  if(pOld == pMax)
                                  then insertH pNew (deleteMaxH mx)
                                  else insertH pMax (actualizarHeap pNew pOld deleteMaxH mx)  
                                        


partida :: [Comando] -> Personaje
-- Dada una lista de comandos valida, describe el ganador del juego que resulta luego de
-- ejecutar la lista. Falla si la lista de comandos no es valida, establecer eficiencia y justificar.
-- Una lista de comandos es valida si su primer elemento es IniciarJuego y los diferentes
-- comandos cumplen las precondiciones de los comandos que representan al momento de
-- ser ejecutados.
-- REQUISITO: La solucion debe realizarse por recursion estructural, para lo cual dicha
-- recursion debe hacerse en una funcion auxiliar,sobre el reverse de la lista dada.
partida cms = elMasPoronga (aplicarComandos (reverse cms))

aplicarComandos :: [Comando] -> EmGame
aplicarComandos [] = error "la lista de comandos no puede ser vacía"
aplicarComandos [c] = case c of 
                    IniciarJuego ps es -> iniciarJuego ps es 
                                     _ -> error "el juego debe empezar con un iniciar juego"
aplicarComandos (c:cs) =  aplicar c (aplicarComandos cs) 

aplicar :: Comando -> EmGame -> EmGame
aplicar (ObtenerEsmeralda p e) em = obtenerEsmeralda em p e 
aplicar (CompetirPor p1 p2 e)  em = ganarEsmeralda em p1 p2 
aplicar (UsarEsmeralda p e )   em = usarEsmeralda em p e 
aplicar (IniciarJuego _ _)     em = error ".."