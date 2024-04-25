module EJ2 where 

type Cadena = [Char]

-- type Linea = (Cadena,Int) -- Se podria asi?
-- Data Linea = "" | Lin (Cadena, Int) -- El "" para interpretar vacio como valor de dato para Linea estaria bien o no hace falta?

data Linea = Lin Cadena Int deriving Show

-- Vacio lo interpreto como "" o como el dato individual Vacio?
vacia :: Linea
vacia = Lin "" 0

-- c = cadena , p = cursor

moverIzq :: Linea -> Linea 
moverIzq (Lin c p) = if  0 < (p - 1) && (p - 1) < (length c - 1) 
                                  then Lin c (p - 1)
                                  else Lin c p

moverDer :: Linea -> Linea 
moverDer (Lin c p) = if  0 < (p + 1) && (p + 1) < (length c + 1) 
                                  then Lin c (p + 1)
                                  else Lin c p

moverIni :: Linea -> Linea
moverIni (Lin c p) = Lin c 0

moverFin :: Linea -> Linea 
moverFin (Lin c p) = Lin c (length c - 1)

insertar :: Char -> Linea -> Linea
insertar caracter (Lin c p) = Lin (insertarAux c p caracter (length c - 1)) (p + 1)
insertarAux (x:cadena) pos caracter indices = if pos == indices then  x:caracter:cadena else x:(insertarAux cadena pos caracter (indices - 1))

borrar :: Linea -> Linea 
borrar (Lin c p) = Lin (borrarAux c p (length c - 1)) (p - 1)
borrarAux (x:cadena) pos indices = if pos == indices then cadena else x:(borrarAux cadena pos (indices - 1))


