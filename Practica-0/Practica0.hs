module P0 where

--5)
--a)
divisors x = if x > 0 then [n | n <- [1..x], x `mod` n == 0] else error "Error numero invalido"

--b)
matches x l = [n | n <- l, n == x]

--c)
cuadrupla :: Int -> [(Int, Int, Int, Int)]
cuadrupla n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a*a + b*b == c*c + d*d]

--d)
unicoV3 l = [x | x <- l, length (matches x l) == 1]

unicoV2' mesa [] = mesa 
unicoV2' mesa (x:mano) = if elem x mesa then unicoV2' mesa mano else unicoV2' (x:mesa) mano 

unicoV2 xs = unicoV2' [] xs

unicoV1' l = [x | (x,pos) <- zip l [1..], not (elem x (drop pos l))]

-- [1,2,3,1,4,5,6,2]

--6)
scalarProduct l1 l2 = sum [x*y | (x,y) <- zip l1 l2]

--7 y 8)

--a)
suma [] = 0
suma (e:l) = e + (suma l)

suma' l = foldr (+) 0 l

--foldr
hacer funcion valorBase [] = valorBase
hacer funcion valorBase (e:cola) = funcion e (hacer funcion valorBase cola)

--b)

alguno :: [Bool] -> Bool
alguno [] = False
alguno (e:l) = if e then True else alguno l

alguno' l = foldr (||) False l

--c)
todos :: [Bool] -> Bool
todos [] = True
todos (e:l) = if (not e) then False else todos l

todos' l = foldr (&&) True l

--d)
--[(a,1),(b,1),(c,2), ... , (z,26)]
buscar c [] = (-1)
buscar c ((x,i):l) = if x == c then i else buscar c l
code c = buscar c (zip (['a'..'n']++['ñ']++['o'..'z']) [1..])

codes [] = []
codes (e:l) = (code e):( codes l)

--e)
restos :: [Int] -> Int -> [Int]
restos l n = [mod e n | e <- l]

restos' [] _ = []
restos' (e:l) n = (mod e n):(restos l n)

--para mi no se puede con foldr
--restos'' l n = foldr () _ l
resto n x = x `mod` n

restos'' l n = map (\x -> x `mod` n) l

--- restos''' 

--f)
cuadrados :: [Int] -> [Int]
cuadrados l = [x*x | x <- l]

cuadrados' [] = []
cuadrados' (e:l) = (e*e):(cuadrados' l)

--tampoco se puede con foldr?
--cuadrados'' l = foldr () 0 l

cuadrados'' l = map (\ x -> x*x) l

--g)
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (e:l) = (length e):longitudes l 

longitudes' l = map length l

longitudes'' l = map (\ x -> length x) l

--h)
--[(1,7),(56,9)]
--(x,y) -> x < (3 * y)
orden [] = []
orden ((x,y):l) = if x < (3 * y) then (x,y):(orden l) else orden l 

orden' l = filter (\ (x,y) -> x < (3 * y)) l 


--i)
pares [] = []
pares (e:l) = if mod e 2 == 0 then e:(pares l) else pares l

pares' l = filter (\x -> mod x 2 == 0) l


--j)

letras [] = []
letras (e:l) = if (e `elem` ['a'..'z']) || (e `elem` ['A'..'Z'])  then e:(letras l) else (letras l)

letras'' l = filter (\ x -> (x `elem` ['a'..'z']) || (x `elem` ['A'..'Z'])) l

--k)
--masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (xss:xs) n = if ((length xss) > n) then  xss:(masDe xs n) else masDe xs n

masDe' xss n = filter (\ xs -> (length xs) > n) xss


