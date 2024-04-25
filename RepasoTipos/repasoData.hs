module Datas where 

type Cadena = [Char]
type Nombre = Cadena
type Dni = Int 
type Edad = Int

data Persona = Pers Nombre Dni Edad deriving Show
p1::Persona
p1 = Pers "Juan" 43646120 20
p2 = Pers "Pedro" 43646120 22 
p3 = Pers "Messi" 325487988 36

verPersona :: Persona -> Cadena 
verPersona (Pers nombre dni edad) = "Nombre: " ++ nombre ++ " |Edad: " ++ show edad ++ " |Dni: " ++ show dni

sonClones :: Persona -> Persona -> Bool 
sonClones (Pers _ dni1 _) (Pers _ dni2 _) = dni1 == dni2