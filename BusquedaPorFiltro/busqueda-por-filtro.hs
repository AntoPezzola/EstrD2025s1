


data Busqueda = B (Map String (Map String Int)) --- nombre producto por atributos 
                    [Filtros]                   --- filtros aplicados a la busqueda

   

{- INV.REP * En el map de los atributos de un producto, el producto no puede tener el mismoa atributo definido dos veces
           * El valor del map principal debe ser el nombre del producto
           * Los productos en el map, deben aplicar a los filtros definidos en la busqueda  
-}



registrar :: String -> Int -> Map String Int -> Busqueda -> Busqueda
-- Costo de aplicaFiltros O(F * log A)
-- Costo de assocM O(log P) siendo P la cantidad total de productos que tiene una busqueda 
registrar nombre precio mpNew (B mp filtros) = 
    let atributoPrecio = assocM "precio" precio mpNew in 
     if (aplicaFiltros atributoPrecio filtros) 
    then B (assocM nombre atributoPrecio mp) filtros
    else B mp filtros


aplicaFiltros :: Map String Int -> [Filtros] -> Bool
-- Costo de aplica con costo O(log A) siendo A la mator cantisad de atributos de un producto en la busqueda
-- por cada elm de mi lista de filtro hago una op de costo O(log m) -> costo final O(F * log A)
aplicaFiltros mp [] = true 
aplicaFiltros mp (f:fs) = (aplica f mp) && aplicaFiltros mp fs 


filtrar :: Filtro -> Busqueda -> Busqueda
-- El costo de filtrar es O(P * (log P + log A)), siendo P la cantidad de productos actuales en la búsqueda (mp) y A la cantidad máxima de atributos por producto.
-- Esto se debe a que:
-- Obtener las claves del map (keys) tiene costo O(P).
-- La función mpConFiltros recorre todos los productos aplicando el filtro: por cada producto realiza lookupM (O(log P)), aplica (O(log A)) y eventualmente assocM (O(log P)).
-- La construcción del nuevo Busqueda y el agregado del filtro son de costo constante.
Por lo tanto, el costo total se simplifica a O(P * (log P + log A)).
filtrar f (B mp filtros) =let prods = keys mp 
                           mapUpdate = mpConFiltros prods mp f in 
                           B mapUpdate (f:filtros)


mpConFiltros :: [String] ->  Map String Int -> Filtro -> Map String Int
-- Costo de aplica con costo O(log A) siendo A la mator cantisad de atributos de un producto en la busqueda
-- Costo de assocM y lookupM O(log P) siendo P la cantidad total de productos que tiene una busqueda 
-- por cada elem de mi recursion hago (P * (log p + log A)) siendo P la cantidad de nombres de productos de mi lista
mpConFiltros [] _ _ = emptyM
mpConFiltros (p:ps) mp filtro =  case lookupM p mp of 
                                  Just atributos -> 
                                                    let mapUpdate = mpConFiltros ps mp filtros  in 
                                                    if (aplica filtro atributos)
                                                    then assocM p atributos mapUpdate
                                                    else mapUpdate
                                              
                                  Nothing -> error "No existe ese producto en la busqueda"


-------------- COMO USUARIO -----------------------------

siguientesN :: Busqueda -> Int -> [(String, Int)]
-- Costo de siguiente O(P + log P + log A)
-- Costo de siguientesN O(n * (P + log P + log A))
siguientesN b 0 = []
siguientesN b n =  let (maybe, busqueda) = siguiente b  in 
                        case maybe of 
                        Just (s,i) -> (s,i) : siguientesN busqueda (n-1)
                        Nothing -> []