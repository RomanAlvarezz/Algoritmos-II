module Clases where

data Persona = Pers { nombre::[Char], dni::Int } deriving Show

--
instance Eq Persona where 
    (==) p1 p2 = dni p1 == dni p2 