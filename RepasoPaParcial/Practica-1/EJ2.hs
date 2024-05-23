module EJ2 where

l1 = [(x,y) | x <- [1,2] , y <- [3,4,5], x + y > 5]

--a)
--cambios :: [a] -> [Int]
cambios' l = [xpos | (x, xpos) <- zip l [0..], (y, ypos) <- zip l [0..], xpos == ypos - 1 && x /= y]

--b)
oblongo = [x * (x - 1) | x <- [2..30] ]

--c)
--abundantes

--d)
repitoElemento _ 0 = []
repitoElemento c n = c : repitoElemento c (n - 1)

eco xs = concat [ repitoElemento c pos | (c,pos) <- zip xs [1..]]

--e)
euler n = sum [x | x <- [1..(n-1)], x `mod` 3 == 0 || x `mod` 5 == 0]

--f)
expandir xs = concat [repitoElemento x x | x <- xs]