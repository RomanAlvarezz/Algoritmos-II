module EJ2 where 

type Cadena = [Char]
type Cursor = Int
data Linea = Lin Cadena Cursor deriving Show

l1 = Lin "Hola" 2
l2 = moverDer l1
l3 = moverDer l2 
l4 = moverDer l3

-- Vacio lo interpreto como ""
vacia :: Linea
vacia = Lin "" 0

-- c = cadena , p = cursor

moverIzq :: Linea -> Linea 
moverIzq (Lin c p) = if  0 <= (p - 1) && c /= "" then Lin c (p - 1) else Lin c p

moverDer :: Linea -> Linea 
moverDer (Lin c p) = if  (p + 1) <= length c && c /= "" then Lin c (p + 1) else Lin c p

moverIni :: Linea -> Linea
moverIni (Lin c p) = Lin c 0

moverFin :: Linea -> Linea 
moverFin (Lin c p) = Lin c (length c)

insertar :: Char -> Linea -> Linea
insertar c (Lin cs p) = Lin (ins c p cs) (p + 1)

ins :: Char -> Int -> [Char] -> [Char]
ins c 0 cs = c:cs 
ins c n (x:cs) = x:(ins c (n - 1) cs) 

borrar :: Linea -> Linea 
borrar (Lin cs p) = Lin (borrAux cs p) (p - 1) --Si le agrego los reverse es como si borrara normal, es decir no con la tecla suprimir


borr 0 (x:cs) = cs   --Aca es como si borrar con suprimir
borr n (x:cs) = x:(borr (n - 1) cs) 

borrAux (x:xs) cur = if cur == 0 then xs else x: borrAux xs (cur - 1)

{-
--Aca intente hacer la funcion borr pero que no borre con suprimir pero salio mal
--rotar 0 _ xs = xs
--rotar n (e:xs) = (rotar (n - 1) xs) ++ [e]
--rotar cursor (e:xs) = rotar (cursor - 1) (xs ++ [e]) 

-- borr' len cursor (e:xs) | cursor - 1 == 0 = borr' (len - 1) (cursor - 1) xs
--                         | otherwise = borr' (len - 1) (cursor - 1) (xs ++ [e]) 

-- borr' 1 _ xs = reverse xs
-- borr' len 0 (e:xs) = borr' (len - 1) (-1) xs
-- borr' len cursor (e:xs) =  borr' (len - 1) (cursor - 1) (xs ++ [e]) 
-}




