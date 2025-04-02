sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria (ns)

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) =  (n + 1) : sucesores ns   

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs 

disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs 

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece x (n:ns) = x == n || pertenece x ns 
                    
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones x (n:ns) = if x == n 
                       then 1 + apariciones x ns 
                       else apariciones x ns 

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x:xs) = if n > x
                       then x : losMenoresA n xs 
                       else losMenoresA n xs 

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (xs:xss) = if (longitud xs) > n 
                                then xs : lasDeLongitudMayorA n xss 
                                else lasDeLongitudMayorA n xss 

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] n = [n]
agregarAlFinal (x:xs) n = x : agregarAlFinal xs n 

agregar :: [a] -> [a] -> [a]
agregar xs ys = xs ++ ys 

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x 

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys = ys
zipMaximos xs [] = xs  
zipMaximos (n:ns) (x:xs) = elMaximoEntre n x : zipMaximos ns xs

elMaximoEntre :: Int -> Int -> Int
elMaximoEntre n y = if n > y
                    then n 
                    else y 


elMinimo :: Ord a => [a] -> a
elMinimo [] = error "la lista no debe ser vacia"
elMinimo [a] = a 
elMinimo (x:xs) = if x < elMinimo xs
                  then x 
                  else elMinimo xs  


-- =============== RECURSIÓN SOBRE NUMEROS ================

factorial :: Int -> Int
factorial 0 = 1 
factorial n = n * factorial (n-1)  

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 y = []
repetir n y = y : repetir (n-1) y 

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs  


sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 ls = ls 
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs 


-- ================ REGISTROS ===============

data Persona = P String Int 
   deriving Show

persona1 = P "Juan" 10 
persona2 = P "Luis" 6

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (p:ps) = if edad p > n 
                    then p : mayoresA n ps 
                    else mayoresA n ps 

promedioEdad :: [Persona] -> Int 
promedioEdad [] = error "Debe haber al menos una persona en la lista"
promedioEdad ps = div (sumarEdades ps) (longitud ps)

sumarEdades :: [Persona] -> Int
sumarEdades [] = 0
sumarEdades (p:ps) = edad p + sumarEdades ps 

elMasViejo :: [Persona] -> Persona
elMasViejo (p:ps) = if estaVacia ps 
                    then p 
                    else laPersonaMasVieja p (elMasViejo ps) 


laPersonaMasVieja :: Persona -> Persona -> Persona
laPersonaMasVieja p1 p2 =  if edad p1 > edad p2 
                            then p1
                            else p2

estaVacia :: [a] -> Bool 
estaVacia [] = True
estaVacia [_] = False 

-- ================== POKEMON ===============0

data TipoDePokemon = Agua | Fuego | Planta
   deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int
   deriving Show
data Entrenador = ConsEntrenador String [Pokemon]
   deriving Show

pokemon1 = ConsPokemon Agua 50
pokemon2 = ConsPokemon Fuego 70
pokemon3 = ConsPokemon Planta 40
pokemon4 = ConsPokemon Planta 80
pokemon5 = ConsPokemon Fuego 55

entrenador1 = ConsEntrenador "Ash" [pokemon1, pokemon2, pokemon4]
entrenador2 = ConsEntrenador "Misty" [pokemon3, pokemon4]
entrenador3 = ConsEntrenador "Brock" [pokemon5]

entrenador4 = ConsEntrenador "Gary" []

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ pks) = longitud pks 

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tpk (ConsEntrenador _ pks) = losPokemonesDeTipo tpk pks 

losPokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> Int
losPokemonesDeTipo t [] = 0
losPokemonesDeTipo t (pk:pks) = if esDelMismoTipo t (tipoPokemon pk)
                                then 1 + losPokemonesDeTipo t pks 
                                else losPokemonesDeTipo t pks 

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (ConsPokemon t _ ) = t

unoSi :: Bool ->  Int
unoSi True = 1
unoSi False = 0

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDelMismoTipo Agua Agua     = True
esDelMismoTipo Fuego Fuego   = True
esDelMismoTipo Planta Planta = True
esDelMismoTipo   _     _     = False

superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon t e) (ConsPokemon t1 e1) =  primeroSuperaASegundo t t1

primeroSuperaASegundo:: TipoDePokemon -> TipoDePokemon -> Bool
primeroSuperaASegundo Agua Fuego = True
primeroSuperaASegundo Fuego Planta = True
primeroSuperaASegundo Planta Agua = True
primeroSuperaASegundo t1 t2 = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = cuantosDeTipo_De_LeGananATodosLosDe_' t (losPokemonesDe e1) (losPokemonesDe e2)

cuantosDeTipo_De_LeGananATodosLosDe_' :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int 
cuantosDeTipo_De_LeGananATodosLosDe_' t [] _ = 0
cuantosDeTipo_De_LeGananATodosLosDe_' t (pk:pks) pks2 = if leGanaATodos pk pks2  
                                                      then 1 + cuantosDeTipo_De_LeGananATodosLosDe_' t pks pks2
                                                      else cuantosDeTipo_De_LeGananATodosLosDe_' t pks pks2
losPokemonesDe :: Entrenador -> [Pokemon]
losPokemonesDe (ConsEntrenador _ pks) = pks

leGanaATodos :: Pokemon -> [Pokemon] -> Bool 
leGanaATodos p [] = True
leGanaATodos p (pk:pks) = superaA p pk && leGanaATodos p pks 


esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ pks) = hayAlMenosDeUnTipo Agua pks && hayAlMenosDeUnTipo Fuego pks && hayAlMenosDeUnTipo Planta pks

hayAlMenosDeUnTipo :: TipoDePokemon ->  [Pokemon] -> Bool
hayAlMenosDeUnTipo t [] = False
hayAlMenosDeUnTipo t (pk:pks) = esDelMismoTipo (tipoPokemon pk) t || hayAlMenosDeUnTipo t pks


-- ======= PROYECTOS ============

data Seniority = Junior | SemiSenior | Senior
   deriving Show
data Proyecto = ConsProyecto String
   deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
   deriving Show
data Empresa = ConsEmpresa [Rol]

   -- Definición de algunos proyectos
proyecto1 = ConsProyecto "Sistema de Ventas"
proyecto2 = ConsProyecto "Plataforma de E-learning"
proyecto3 = ConsProyecto "App de Finanzas"

-- Definición de roles en los proyectos
rol1 = Developer Junior proyecto1
rol2 = Developer SemiSenior proyecto2
rol3 = Developer Senior proyecto3
rol4 = Management Senior proyecto1
rol5 = Management SemiSenior proyecto2

-- Definición de una empresa con diferentes roles
empresa1 = ConsEmpresa [rol1, rol2, rol3, rol4, rol5]

-- Empresa sin empleados
empresa2 = ConsEmpresa []


proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa p) = proyectos' p 

proyectos' :: [Rol] -> [Proyecto]
proyectos' [] = []
proyectos' (r:rs) = proyectoDe r : proyectos' rs

proyectoDe :: Rol -> Proyecto 
proyectoDe (Developer _ p1) = p1
proyectoDe (Management _ p2) = p2 

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa p) pys = losDevSenior' p pys 

losDevSenior' :: [Rol] -> [Proyecto] -> Int
losDevSenior' rls pys = cantConProyecto rls pys 

cantConProyecto :: [Rol] -> [Proyecto] -> Int 
cantConProyecto [] _ = 0 
cantConProyecto (r:rs) pys = if pertenceALosProyectos r pys && esRolSenior r 
                             then 1 + cantConProyecto rs pys 
                             else cantConProyecto rs pys  

pertenceALosProyectos :: Rol -> [Proyecto] -> Bool   
pertenceALosProyectos (Developer _ p) ps  = tieneElProyecto p ps
pertenceALosProyectos (Management _ p) ps = tieneElProyecto p ps

tieneElProyecto :: Proyecto -> [Proyecto] -> Bool 
tieneElProyecto p [] = False 
tieneElProyecto p (p1:ps) = esElMismoProyecto p p1 || tieneElProyecto p ps 

esElMismoProyecto :: Proyecto -> Proyecto -> Bool
esElMismoProyecto (ConsProyecto np1) (ConsProyecto np2) = np1 == np2  


esRolSenior :: Rol -> Bool
esRolSenior  (Developer s _) = esSenior s 
esRolSenior  (Management s1 _) = esSenior s1 

esSenior :: Seniority -> Bool 
esSenior Senior = True
esSenior _      = False

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn (p:ps) e = unoSi(tieneElProyecto p (proyectos e)) + cantQueTrabajanEn ps e 

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa r) = losRolesConProyectosAsignados r 

losRolesConProyectosAsignados :: [Rol] -> [(Proyecto, Int)]
losRolesConProyectosAsignados [] = []
losRolesConProyectosAsignados (r:rs) = consolidar r (losRolesConProyectosAsignados rs) 

consolidar :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)]
consolidar r [] = [(proyectoDeRol r , 1)]
consolidar r (t:ts) = if nombreP (proyectoDeRol r) == nombreP (fst t)
                      then (fst t , snd t + 1) : ts 
                      else t : consolidar r ts 

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ p) = p
proyectoDeRol (Management _ p) = p

nombreP :: Proyecto -> String
nombreP (ConsProyecto s) = s 