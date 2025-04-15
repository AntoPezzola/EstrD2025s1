-- ============ CELDAS CON BOLITAS ==============

data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show


celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda2 = CeldaVacia
celda3 = Bolita Azul (Bolita Azul CeldaVacia)



nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita cc cel) = unoSi (esElMismoColor c cc) + nroBolitas c cel 

unoSi :: Bool ->  Int
unoSi True = 1
unoSi False = 0

esElMismoColor :: Color -> Color -> Bool 
esElMismoColor Azul Azul = True
esElMismoColor Rojo Rojo = True
esElMismoColor _ _ = False

poner :: Color -> Celda -> Celda
poner col CeldaVacia = Bolita col CeldaVacia
poner col (Bolita c cel) = Bolita col (poner c cel) 

sacar :: Color -> Celda -> Celda
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita col cel) = if (esElMismoColor c col) 
                            then cel 
                            else Bolita c (sacar c cel)   ---- DUDA 


ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c celda = celda
ponerN n c celda = poner c (ponerN (n-1) c celda) 


-- ================ CAMINO HACIA EL TESORO =============== 

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

-- Camino vacío
camino1 = Fin

-- Camino con un solo cofre vacío
camino2 = Cofre [] Fin

-- Camino con un cofre que tiene un solo Tesoro
camino3 = Cofre [Tesoro] Fin

-- Camino con un cofre con Cacharro y Tesoro, seguido por un camino vacío
camino4 = Cofre [Cacharro, Tesoro] Fin

-- Camino con un cofre lleno, seguido por un Nada, luego un cofre vacío
camino6 = Cofre [Cacharro] (Nada (Cofre [] Fin))

-- Camino de varios pasos con mezcla
camino7 = Cofre []
             (Nada 
                 (Nada
                     (Cofre [Cacharro, Tesoro] Fin)))

-- Camino más complejo, tipo cadena larga
camino9 = Nada (
             Cofre [Cacharro] (
             Cofre [Tesoro, Tesoro] (
             Nada (
             Cofre [] (
             Cofre [Cacharro] Fin)))))


hayTesoro :: Camino -> Bool
hayTesoro (Fin)            = False
hayTesoro (Cofre objs c) =  tieneTesoro objs || hayTesoro c 
hayTesoro (Nada c)       = hayTesoro c 

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (o:os) = esTesoro o || tieneTesoro os 

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False


pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Fin)          =  error "Debe haber al menos un tesoro"
pasosHastaTesoro (Cofre objs c) =  if tieneTesoro objs 
                                   then 0
                                   else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c)       =  1 + pasosHastaTesoro c 


hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn  n camino =  pasosHastaTesoro camino == n 


alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 (Fin)          = True      --- ===== DUDA 
alMenosNTesoros n (Fin)          = False 
alMenosNTesoros n (Cofre objs c) = if tieneTesoro objs
                                   then alMenosNTesoros (n-1) c 
                                   else alMenosNTesoros n c   
alMenosNTesoros n (Nada c)       = alMenosNTesoros n c 


-- ============ ARBOLES BINARIOS ==============


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

    -- Árbol vacío
arbol1 = EmptyT

-- Árbol con un solo nodo
arbol2 = NodeT 5 EmptyT EmptyT

-- Árbol con raíz y un hijo izquierdo
arbol3 = NodeT 10 (NodeT 5 EmptyT EmptyT) EmptyT

-- Árbol con raíz y un hijo derecho
arbol4 = NodeT 10 EmptyT (NodeT 15 EmptyT EmptyT)

-- Árbol con raíz y dos hijos
arbol5 = NodeT 10 (NodeT 10 EmptyT EmptyT) (NodeT 10 EmptyT EmptyT)

-- Árbol más profundo
arbol6 = NodeT 1
            (NodeT 2
                (NodeT 4 EmptyT EmptyT)
                (NodeT 5 EmptyT  (NodeT 5 EmptyT  (NodeT 5 EmptyT  (NodeT 5 EmptyT EmptyT)))))
                                            (NodeT 3
                                                EmptyT
                                                (NodeT 6 EmptyT (NodeT 6 EmptyT EmptyT)))
                

sumarT :: Tree Integer -> Integer
sumarT EmptyT = 0
sumarT (NodeT n n1 n2) = n + sumarT n1 + sumarT n2

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ n1 n2) = 1 + sizeT n1 + sizeT n2  


mapDobleT :: Tree Integer -> Tree Integer
mapDobleT EmptyT          = EmptyT 
mapDobleT (NodeT n n1 n2) = NodeT (n*2) (mapDobleT n1) (mapDobleT n2) 


perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT          = False
perteneceT x (NodeT n n1 n2) = x == n || perteneceT x n1 || perteneceT x n2  

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x EmptyT          = 0
aparicionesT x (NodeT n n1 n2) = unoSi(x == n) + aparicionesT x n1 + aparicionesT x n2 

leaves :: Tree a -> [a]
leaves EmptyT          = []
leaves (NodeT n EmptyT EmptyT) = [n]
leaves (NodeT n n1 n2) = leaves n1 ++ leaves n2 

heightT :: Tree a -> Int
heightT EmptyT          = 0
heighT (NodeT e t1 t2) = 1 + max (heighT t1) (heighT t2)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT e t1 t2) = NodeT e  (mirrorT t2)  (mirrorT t1)

toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT e t1 t2) = toList t1 ++ [e] ++ toList t2

levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN 0 (NodeT e _ _) = [e] 
levelN n (NodeT e t1 t2) = (levelN (n-1) t1) ++ (levelN (n-1) t2)   


listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = [[]]
listPerLevel (NodeT e t1 t2) = [e] : unirNiveles(listPerLevel t1) (listPerLevel t2) 

unirNiveles :: [[a]] -> [[a]] -> [[a]]
unirNiveles [] ys = ys
unirNiveles xs [] = xs 
unirNiveles (xs:xss) (ys:yss) = (xs ++ ys) : (unirNiveles xss yss) 


ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT e t1 t2) = e : ramaMasLarga' (ramaMasLarga t1) (ramaMasLarga t2)


ramaMasLarga' :: [a] -> [a] -> [a]
ramaMasLarga' x y = if (longitud x) > (longitud y) 
                    then x
                    else y 


longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
    deriving Show 

eval1 = Prod (Valor 3) (Valor 10)


eval :: ExpA -> Int
eval (Valor n)   = n
eval (Sum n n1)  = (eval n) + (eval n1) 
eval (Prod n n1) = (eval n)*(eval n1)
eval (Neg n    ) = -(eval n) 

simplificar :: ExpA -> ExpA
simplificar (Valor n) = Valor n
simplificar (Sum e1 e2) = sumaSimplificada (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2) = prodSimplificada (simplificar e1) (simplificar e2)
simplificar (Neg e) = negSimpli (simplificar e)

sumaSimplificada :: ExpA -> ExpA -> ExpA
sumaSimplificada (Valor 0) e = e
sumaSimplificada e (Valor 0) = e
sumaSimplificada e1 e2 = Sum e1 e2

prodSimplificada :: ExpA -> ExpA -> ExpA
prodSimplificada (Valor 0) e = Valor 0
prodSimplificada e (Valor 0) = Valor 0
prodSimplificada (Valor 1) e = e
prodSimplificada e (Valor 1) = e
prodSimplificada e1 e2 = Prod e1 e2

negSimpli :: ExpA -> ExpA
negSimpli (Neg e) = e
negSimpli e = Neg e