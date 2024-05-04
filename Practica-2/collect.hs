module Collect where

listaEj = [('a',3),('b',5),('a',9),('c',2),('b',7)]

--valoresAsociados :: a -> [(a,b)] -> [b]
valoresAsociados _ [] = []
valoresAsociados k ((c,v):l) | k == c = v:valoresAsociados k l 
                           | otherwise = valoresAsociados k l

borrarClave _ [] = []
borrarClave k ((c,v):l) | k == c = borrarClave k l 
                        | otherwise = (c,v):borrarClave k l

collect [] = [] 
collect ((k,v):l) = (k, v:(valoresAsociados k l) ):collect (borrarClave k l)