module Ejercicio8cho where

--A
divisorss n = if (n<0) then [] else [x | x <- [1..n], n `mod` x == 0]

--B matches, que dados un entero x y una lista de enteros descarta de la lista los elementos distintos a x
matches n l = [x | x <- l, x == n]


--C  unique, que dada una lista xs de enteros, devuelve la lista con los elementos no repetidos de xs.
noMatches n l = [x | x <- l, x /= n]

unicos [] = []
unicos (e:l) = if elem e l then unicos (noMatches e l) else e:(unicos l)
{-- 
unicos [1,2,3,1,4]

1)unicos (1:[2,3,1,4]) if elem 1 [2,3,1,4] then unicos (noMatches 1 l) else 1:(unicos [2,3,1,4])

--}
