module PruebaFolds where

--hacer :: (a -> b -> b) -> b -> [a] -> b
hacer funcion valorBase [] = valorBase
hacer funcion valorBase (c:cola) = funcion c (hacer funcion valorBase cola) 