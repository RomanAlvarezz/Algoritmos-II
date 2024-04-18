module DATA where

data RGB = Red | Green | Blue deriving Show

giveColor:: Int -> RGB
giveColor 1 = Red 
giveColor 2 = Green
giveColor 3 = Blue

data Forma = Circulo Float | Rect Float Float deriving Show
cuadrado :: Float -> Forma 
cuadrado n = Rect n n 

area :: Forma -> Float 
area (Circulo r) = (3.14) * r * r
area (Rect x y) = x * y