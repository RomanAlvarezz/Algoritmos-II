module Funciones where

division :: Int -> Int -> Int
division x y = x `div` y

cuadrado :: Int -> Int
cuadrado x = x*x

funcionComplejaAhrre :: Int -> Int -> Int -> Int -> Int 
funcionComplejaAhrre x y z k = division x y + cuadrado z - cuadrado k

doble :: Int -> Int
doble n = n*2 

dobleMasUno :: Int -> Int
dobleMasUno n = doble n + 1

cuadruple :: Int -> Int 
cuadruple n = n*4

cuadruple' :: Int -> Int
cuadruple' n = doble (doble n)

isEmptyList :: [a] -> Bool
isEmptyList l = if (length l == 0) then True else False 

{-
    Pattern Matching:
    Defino una funcion dos veces, dependiendo de cual sea el argumento
-}

isEmptyList' :: [a] -> Bool
isEmptyList' [] = True
isEmptyList' l = False

unirListas :: [a] -> [a] -> [a] 
unirListas l1 l2 = l1 ++ l2

listaNoAplanada = [[34,6],[89,0,11]]

listaAplanada = concat listaNoAplanada --La funcion concat ya viene en haskell

tresPrimerosElementos :: [a] -> [a]
tresPrimerosElementos l = take 3 l --La funcion take ya viene en haskell

nombres = ["Valentin", "Figal", "Sergio", "asd"]
telefonos = [3416128939, 4986804, 123456789]

listasCombinadas = zip nombres telefonos --La funcion zip ya viene en haskell