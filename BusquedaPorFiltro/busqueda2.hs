data Busqueda = B (Map String (Map String Int)) --- nombre producto por atributos 
                    [Filtros]                   --- filtros aplicados a la busqueda

   

{- INV.REP * En el map de los atributos de un producto, el producto no puede tener el mismoa atributo definido dos veces
           * El valor del map principal debe ser el nombre del producto
           * Los productos en el map, deben aplicar a los filtros definidos en la busqueda  
-}

registrar :: String -> Int -> Map String Int -> Busqueda -> Busqueda
registrar n p mpAtributos (B mp f) = 
                                    if (aplicaFiltros mpAtributos f)
                                  then B (assocM n (assocM 'precio' p mp)) f 
                                  else B mp f 
                                
aplicaFiltros :: Map String Int -> [Filtro] -> Bool
aplicaFiltros _ [] = true 
aplicaFiltros mp (f:fs) = aplica f mp && (aplicaFiltros mp fs)

filtrar :: Filtro -> Busqueda -> Busqueda
filtrar f (B mp fs) = let newF = (f:fs) 
                          productos = keys mp in 
                   B (newMapFiltrados productos mp) newF


newMapFiltrados :: [String] -> Map String Int -> -Filtro -> Map String Int
newMapFiltrados [] mp = emptyM
newMapFiltrados (s:ss) mp f = case lookupM s mp of 
                          Just atributos -> if(aplica atributos f) 
                                         then assocM s atributos (newMapFiltrados ss mp f)
                                         else newMapFiltrados ss mp f
                            Nothing -> error "no existe un producto con ese nombre"

siguiente :: Busqueda -> (Maybe (String, Int), Busqueda) 
siguiente (B mp fs) = let prods = keysM mp in 
                         (busquedaSinProd prods mp, (B mp fs))

busquedaSinProd :: [String] -> Busqueda -> (Maybe (String, Int), Busqueda)
busquedaSinProd [] (B mp fs)     = (Nothing, (B mp fs))
busquedaSinProd (s:ss) (B mp fs) =
                          case lookupM s mp of 
                            Just atributo -> case lookupM 'precio' atributos
                                            Just precio -> (Just (s,precio), (B (deleteM s mp), fs))
                                            Nothing -> error "el prod no tiene precio"

siguientesN :: Busqueda -> Int -> [(String, Int)]
siguientesN b 0 = []
siguientesN b n = let (maybeProd, busqueda) = siguiente b in 
                case maybeProd of 
                Nothing -> []
                Just (s,i) -> (s,i) : siguientesN busqueda (n-1)