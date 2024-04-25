module Types where 

type Cadena = [Char]
type Nombre = Cadena
type Dni = Int 
type Persona = (Nombre, Dni)

sonClones :: Persona -> Persona -> Bool 
sonClones (_, dni1) (_,dni2) = dni1 == dni2