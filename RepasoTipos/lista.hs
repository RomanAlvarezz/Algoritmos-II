module Lista where

data Lista a = Lis a (Lista a) | Vacio deriving Show

cabeza Vacio = error "Lista Vacia"
cabeza (Lis x xs) = x

longitud Vacio = 0
longitud (Lis x xs) = 1 + longitud xs