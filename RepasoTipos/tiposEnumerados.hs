module Enumerados where

data Color = Rojo | Verde | Azul deriving Show
data Temperatura = Frio | Templado |Calor deriving Show
data Estacion = Primavera | Verano | Otonio | Invierno deriving Show 

tiempo :: Estacion -> Temperatura 
tiempo Primavera = Templado
tiempo Verano = Calor
tiempo Otonio = Templado 
tiempo Invierno = Frio 

data Forma = Circulo { radio::Float } | Rectangulo { altura::Float, ancho::Float } deriving Show

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Rectangulo alt anch) = alt * anch
 