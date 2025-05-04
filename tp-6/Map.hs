module Map
  (Map, emptyM, assocM , lookupM , deleteM , keys)
where 

data Map k v = M [(k,v)]
  {- INV.REP.: en M kvs, no hay claves repetidas en kvs -}
  deriving (Show)


emptyM :: Map k v 
assocM :: Eq k => k -> v -> Map k v -> Map k v
--Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
--Propósito: borra una asociación dada una clave.
keys :: Map k v -> [k]
--Propósito: devuelve las claves del map

emptyM = M []
assocM k v (M kvs) = M (asociar k v kvs)
lookupM k (M kvs)  = buscarK k kvs 
deleteM k (M kvs)  = M (borrarK k kvs)
keys (M kvs) = lasClaves kvs


asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
-- Costo O(n) ya que hace una op constante por cada elem de la lista en peor caso
-- n es el tamaño de la lista de kvs
asociar k v []            = [(k,v)]
asociar k v ((k',v'):kvs) = if k==k'
                          then (k',v):kvs  
                          else (k',v'):asociar k v kvs
 
buscarK :: Eq k => k -> [(k,v)] -> Maybe v
-- Costo O(n) ya que es una op constante por cada elem de la lista, en peor caso recorre toda la lista
-- donde n es el tamaño de la lista de kvs
buscarK k []          = Nothing
buscarK k ((k',v): kvs) = if k==k' 
                        then Just v 
                        else buscarK k kvs

borrarK :: Eq k => k -> [(k,v)] -> [(k,v)]
-- Costo O(n) ya que es una op constante por cada elem de la lista, en peor caso recorre toda la lista
-- donde n es el tamaño de la lista de kvs
borrarK k []         = []
borrar k ((k',v): kvs) = if (k==k') 
                       then kvs 
                       else (k',v) : borrarK k kvs 

lasClaves :: [(k,v)] -> [k]
-- Costo O(n) ya que es una op constante por cada elem de la lista, en peor caso recorre toda la lista
-- donde n es el tamaño de la lista de kvs
lasClaves []        = []
lasClaves ((k,v):kvs) =  k : lasClaves kvs  


