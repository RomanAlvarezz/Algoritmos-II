module EJ5 where

hacer funcion valorBase [] = valorBase
hacer funcion valorBase (c:cola) = funcion c (hacer funcion valorBase cola) 

--A)
--map (\ x -> x*2) [1,2,3] esta
--map (*2) [1,2,3] y esta dan el mismo resultado

map' funcion [] = []
map' funcion (e:lista) = (funcion e):(map' funcion lista) 

mapPrima :: (a -> b) -> [a] -> [b]
mapPrima f = foldr (\x acc -> f x : acc) [] -- ni idea que esta pasando aca
