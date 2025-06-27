import EscuelaDeMagia


hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
-- Propósito: Retorna todos los hechizos aprendidos por los magos.
-- Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos edm = hechizosAprendidos' (magos edm) edm 
 

hechizosAprendidos' :: [Nombre] -> EscuelaDeMagia  -> Set Hechizo
-- Costo de unionS O(H log H) siendo h la cantidad de hechizos en el set
-- Costo de hechizosDe O(log M) siendo M la cantidas de magos en la escuela
-- Costo hechizosAprendidos' (M * (h log h + log m)) ya que hago una funcion de costo log h y log m por cada elem de la recu M siendo la longuitud de la lista de nombres 
hechizosAprendidos' [] _       = emptyS
hechizosAprendidos' (n:ns) edm = unionS (hechizosDe n edm) (hechizosAprendidos' ns edm)  


 hayUnExperto :: EscuelaDeMagia -> Bool
-- Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
-- Eficiencia: O(log M)
-- costo de egresarUno O(log M) siendo m la cantidad de magos en la escuela
-- costo de leFaltanAprenderO(log M) siendo m la cantidad e magos en la escuela
-- Costo de hayUnExperto O(log M)  
hayUnExperto edm = let (mago, edm') = egresarUno edm in 
                    leFaltanAprender (nombre mago) edm' == 0


egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
-- Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos
-- magos.
-- Eficiencia: O(M log M)
egresarExpertos edm = if (hayUnExperto edm) 
                     then
                        let (e, edm') = egresarUno edm 
                            (magos, edmFinal) = egresarExpertos edm'
                         in (e:magos, edmFinal)
                    else ([], edm)

