module EJ6 where

-- EJ A
suma :: [Int] -> Int
suma [] = 0
suma [a] = a
suma (e:l) = e + suma l

-- EJ B
alguno :: [Bool] -> Bool
alguno [] = False
alguno (e:l) = if e then True else alguno l

-- EJ C
todos :: [Bool] -> Bool
todos [] = True
todos (e:l) = if (not e) then False else todos l

-- EJ E
restos :: [Int] -> Int -> [Int]
restos l e = [mod x e | x <- l]

--EJ F
cuadrados :: [Int] -> [Int]
cuadrados l = [x*x | x <- l]

--EJ G
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (e:l) = (length e):longitudes l

-- EJ H
orden :: [(Int, Int)] -> [(Int, Int)]
primerElem (x,_) = x
segundoElem (_,x) = x
orden [] = []
orden (e:l) = if (primerElem e < (segundoElem e) * 3) then e:orden l else orden l

-- EJ I
pares [] = []
pares (e:l) = if mod e 2 == 0 then e:(pares l) else pares l

-- EJ K
masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (xss:xs) n = if ((length xss) > n) then  xss:(masDe xs n) else masDe xs n

