

nuevo :: Organizador
-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
-- Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
-- de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
-- no está vacío.
-- Eficiencia: no hay ninguna garantía de eficiencia.
todosLosProgramas :: Organizador -> [Checksum]
-- Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
autoresDe :: Organizador -> Checksum -> Set Persona
-- Propósito: denota el conjunto de autores que aparecen en un programa determinado.
-- Precondición: el Checksum debe corresponder a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
programasDe :: Organizador -> Persona -> Set Checksum
-- Propósito: denota el conjunto de programas en los que participó una determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
--Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
-- programas del organizador, y C la cantidad total de programas.
nroProgramasDePersona :: Organizador -> Persona -> Int
-- Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.


data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

-- -- INV.REP :
--             * las personas que estan en el set asociadas a la clave de un Checksum debe estar como clave en el segundo map
--             * los Checksum que estan como clave en el primer map, deben estar como valor en el segundo map 
            
nuevo :: Organizador
nuevo = MkO emptyM emptyM

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma  (MkO checkMp perMp) ch setP = MkO (assocM ch setP checkMp) (agregarMp (setToList setP) ch perMp) 

agregarMp :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
-- setToList setP tiene n elementos (n personas).
-- Por cada persona, hago:
-- lookupM (log P)
-- addS (log C)
-- assocM (log P)
-- entonces O(n (log c + log p)) ya que por cada elem de mi lista n, hago algo de costo (log c + log p)
agregarMp  [] ch mp = mp 
agregarMp [p:ps] ch mp = case lookupM p mp of 
                         Just ch' -> 
                            let chNew = addS ch ch' in 
                            assocM p chNew (agregarMp ps ch mp) 
                         Nothing -> assocM p (addS ch (emptyS)) mp 
                    
todosLosProgramas :: Organizador -> [Checksum]
-- tiene costo O(C) ya que es el costo de domM
todosLosProgramas (MkO checkMp _) = domM checkMp


autoresDe :: Organizador -> Checksum -> Set Persona
-- Costo O(log C) donde C es la cantidad total de programas del organizador.
autoresDe (MkO checkMp _) ch = case lookupM ch checkMp of 
                                    Just p' -> p' 
                                    Nothing -> error "Debe existir el checksum"

programasDe :: Organizador -> Persona -> Set Checksum
-- Costo O(log P) donde P es la cantidad total de personas del organizador.
programasDe (MkO _ perMp) c =  case lookupM c perMp of 
                                    Just c' -> c' 
                                    Nothing -> error "Debe existir el la persona"

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
-- costo de programarDe O(log P) donde P es la cantidad total de personas del organizador.
-- costo de intersection (C log C)  C es la cantidad total de programas del organizador.
-- costo de sizeS O(1)
-- costo de programaronJuntas O(log P + C log C)
programaronJuntas (MkO checkMp perMp) p1 p2 = 
                                         let p1' = programasDe (MkO checkMp perMp) p1 
                                             p2' = programasDe (MkO checkMp perMp) p2 in 
                                        sizeS (intersection p1' p2')  > 0

------------------------------------------COSTO ERRONEO-------------------------------------------------
-- programaronJuntas' :: [Checksum] -> Map Checksum (Set Persona) -> Persona -> Persona -> Bool
-- -- costo de lookupM O(log C) donde C es la cantidad total de programas del organizador.
-- -- costo belongs O(log P) donde P es la canridad total de personas del organizador.
-- -- costo de programaronJuntas' O(C * (log C + log P)) ya que por cada elem de la lista cago una op de costo (log C + log P)
-- programaronJuntas' [] mp p1 p2 = False
-- programaronJuntas' (ch:chs) mp p1 p2 = case lookupM ch mp of 
--                                        Just sp' -> if (belongs p1 sp') && (belongs p2 sp')
--                                                     then True
--                                                     else programaronJuntas' chs mp p1 p2 
--                                        Nothing -> programaronJuntas' chs mp p1 p2 


nroProgramasDePersona :: Organizador -> Persona -> Int
-- costo de lookupM es O(log P) donde P es la cantidad total de personas del organizador.
-- cost de sizeS O(1) 
-- costo de nroProgramasDePersona O(log P) donde P es la cantidad total de personas del organizador.
nroProgramasDePersona (MkO checkMp perMp) p = case lookupM p perMp of 
                                              Just setCh -> sizeS setCh
                                              Nothing -> error "La persona no existe en el organizador"
