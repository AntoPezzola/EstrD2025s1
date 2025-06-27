
data GBBoard = GBB Coord                            -- tamaño del tablero
                  (History Coord)                   -- la historia del cabezal
                  (Map Coord (History Cell))        -- la historia de cada celda modificada
                  (History ChangeType)              -- la historia de todos los cambios 

type Coord = (Int, Int)
type Cell = (Int, Int, Int, Int)
type ChangeType =  NoChange             -- El valor incial de la historia del cambio
                 | ChangeCell Coord     -- Un cambio en la celda de la coordenada adada 
                 | HeadChange           -- Un cambio en la ps del cabezal 

-- INV.REP : 