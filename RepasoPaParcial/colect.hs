module Colect where

l1 = [('a',3),('b',5),('a',9),('c',2),('b',7)]

collect [] = []
collect ((l,n):xs) = (l , n:(listClave l xs)) : collect (borroClave l xs)

listClave _ [] = [] 
listClave l ((c,v):xs) = if l == c then v : listClave l xs else listClave l xs

borroClave _ [] = []
borroClave l ((c,v):xs) = if l == c then borroClave l xs else (c,v) : borroClave l xs
