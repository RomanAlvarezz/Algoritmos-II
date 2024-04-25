module Datas where 

type Cadena = [Char]
type Nombre = Cadena
type Dni = Int 
type Edad = Int

data Persona = Pers { nombre::Cadena, dni::Int, edad::Int } deriving Show
p1::Persona
p1 = Pers "Juan" 43646120 20
p2 = Pers { nombre="Pedro", dni=43646120, edad=22 } 
p3 = Pers "Messi" 325487988 38

verPersona :: Persona -> Cadena 
verPersona (Pers nombre dni edad) = "Nombre: " ++ nombre ++ " |Edad: " ++ show edad ++ " |Dni: " ++ show dni

sonClones :: Persona -> Persona -> Bool 
sonClones p1 p2 = dni p1 == dni p2

esJoven :: Persona -> Bool
esJoven (Pers _ _ edad) = edad < 37