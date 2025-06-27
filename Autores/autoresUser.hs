programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos
--  programas en las que las personas programaron juntas.
-- Costo de programasDe O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
-- Costo de intersection O(C log C)  en peor caso, donde C es la cantidad total de checksum del organizador.
-- costo O(log P + C log C)
programasEnComun p1 p2 org = intersection (programasDe p1 org) (programasDe p2 org) 


esUnGranHacker :: Organizador -> Persona -> Bool
-- Propósito: denota verdadero si la persona indicada aparece como autor de todos los
--  programas del organizador.
-- costo todosLosProgramas y length O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
-- costo nroProgramasDePersona O(log P) en peor caso, donde P es la cantidad de personas del organizador.
-- 
esUnGranHacker org p = let listaDeCh = todosLosProgramas org in 
                        (nroProgramasDePersona org p) == (length listaDeCh)    
                        