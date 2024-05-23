module EJ1 where

--a)
borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

--b)

--c)
serie' [] = [] 
serie' xs = serie' (borrarUltimo xs) ++ [xs] 

serie xs = [] : serie' xs

--f)
ror 0 xs = xs
ror n (x:xs) = (ror (n-1) xs) ++ [x]

--g)
upto n m | n < m = n : (upto (n + 1) m)
         | n == m = [m]
         | otherwise = []

--h)
-- repitoLetra (_,0) = []
-- repitoLetra (c,n) = c : repitoLetra (c, (n - 1))

-- eco' [] = [] 
-- eco' (x:xs) = (repitoLetra x) ++ (eco' xs)

-- eco xs = eco' (zip xs [1..])

letraRepetida _ 0 = []
letraRepetida c n = c : letraRepetida c (n - 1)

ecco' [] = []
ecco' ((c,n):xs) = letraRepetida c n ++ ecco' xs 

ecco xs = ecco' (zip xs [1..])
