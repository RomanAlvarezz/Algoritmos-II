module Prueba1 where

--type simple
type Nombre = [Char]

mostrarNombre :: Nombre -> Nombre
mostrarNombre nom = nom

--type con parametros
type Pair a = (a,a)

mult :: Pair Int -> Int
mult (m,n) = m*n

copy :: a -> Pair a 
copy x = (x,x)

type Pos = (Int,Int)

origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1,y)

isFivee :: Int -> Bool
isFivee 5 = True 
isFivee _ = False