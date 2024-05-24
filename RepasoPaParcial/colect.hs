module Colect where

l1 = [('a',3),('c',2),('b',5),('a',9),('b',7)]

collect xs = isort (collect' xs)

collect' [] = []
collect' ((l,n):xs) = (l , n:(listClave l xs)) : collect' (borroClave l xs)

listClave _ [] = [] 
listClave l ((c,v):xs) = if l == c then v : listClave l xs else listClave l xs

borroClave _ [] = []
borroClave l ((c,v):xs) = if l == c then borroClave l xs else (c,v) : borroClave l xs

insert (c,v) [] = [(c,v)]
insert (c,v) ((c',v'):xs) = if c < c' then (c,v):(c',v'):xs else (c',v'): insert (c,v) xs 

isort [] = []
isort [x] = [x]
isort (x:xs) = insert x (isort xs)