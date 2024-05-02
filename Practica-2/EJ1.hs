module EJ1 where

--A)
borrarUltimo (e:[]) = []
borrarUltimo (e:l) = e:(borrarUltimo l)

--B)

--C)
ls = [1,2,3]

serie l = []:[ take i l | (_,i) <- zip l [1..] ]

--serie' (e1:l) = [ take i (e1:l) | (_,i) <- zip [0..length l + 1] (e1:e1:l) ]

serie'' (e1:l) = [ take i (e1:l) | (_,i) <- zip (e1:e1:l) [0..]] 

serieR' [] = []
serieR' xs = serieR' (borrarUltimo xs) ++ [xs]
serieR xs = []:serieR' xs

-- POR QUÉ serie' me da mal y serie'' me da bien???

--prueba (e1:l) = (e1:e1:l)
--listaConIndices (e1:l) = zip [0..length l + 1] (e1:e1:l)
-- serie (e1:e2:l) = [e1,e1] : serie (e1:e2:l)
-- serie (e1:e2:e3) = [e1,e2,e2] : serie (e1:e2:e3:l) 

--D)
paresIguales :: Int -> Int -> Int -> Int -> Bool 
paresIguales x y z k = ((x+y) `mod` 2 == 0) && 
                        ((y+z) `mod` 2 == 0) && 
                        ((z+k) `mod` 2 == 0) 

--E)
isoceles :: Int -> Int -> Int -> Bool 
isoceles l1 l2 l3 | l1 == l2 && l3 /= l1 = True 
                  | l2 == l3 && l1 /= l2 = True 
                  | l1 == l3 && l2 /= l1 = True 
                  | otherwise = False

--F)
rotar 0 xs = xs
--rotar n (e:xs) = (rotar (n - 1) xs) ++ [e]
rotar n (e:xs) = rotar (n - 1) (xs ++ [e]) 
ror n xs = if n <= length xs then rotar n xs
            else error "n es mayor que length xs"

--G)
--uptoConstructor n m = if n = m then [m] else uptoConstructor (n-1) m ++ [n-1] 
uptoConstructor n m = if n == m then [m] else n:(uptoConstructor (n + 1) m)
upto n m = if n <= m then uptoConstructor n m else []

--uptoConstructor' n m = if n >= m then [m] else n:(uptoConstructor' (n + 1) m)

--H)
repitoLetra _ 0 = []
repitoLetra letra n = letra:(repitoLetra letra (n-1))

tomopalabra cadena = concat (map (\ (letra,pos) -> repitoLetra letra pos) (zip cadena [1..]))

--longitudes'' l = map (\ x -> length x) l



--ADICIONAL
-- lasts [1,2,3] ---> [[],[3],[2,3],[1,2,3]]
{- Ayudin
serieR' [] = []
serieR' xs = serieR' (borrarUltimo xs) ++ [xs]
serieR xs = []:serieR' xs
-}
--lasts' [3,2,1] ---> [[],[3],[2,3],[1,2,3]]
lasts1' [] = []
lasts1' l = lasts1' (borrarUltimo l) ++ [ reverse l ]
lasts1 l = []:lasts1' (reverse l) 

-- lasts2' [] = []
-- lasts2' l = lasts2' (borrarUltimo l) ++ [ reverse l ]
-- lasts2 l = []:lasts2' l 