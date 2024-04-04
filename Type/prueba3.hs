module Prueba3 where

data Persona = Pers {nombre :: [Char],edad :: Int} deriving Show
p1 = Pers {nombre="Roman",edad=21}
p2 = Pers {nombre="Antonia",edad=21}
p3 = Pers {nombre="Jose",edad=44}

-- data SerVivo = Persona {
--                 nombre :: [Char],
--                 edad :: Int,
--                 sexo :: Char,
--                 donante :: Bool
--                 } | 
--                Perro {
--                 nombre :: [Char],
--                 edad :: Int,
--                 domestico :: Bool
--                } deriving Show 

-- especimen1 = Persona {nombre="Roman", edad=21, sexo='M',donante=True}
-- especimen2 = Perro {nombre="Duque", edad=10, domestico=True}

-- infoCompletaSerVivo :: SerVivo -> [Char]
-- infoCompletaSerVivo (Persona nombre edad sexo _) = "Esta persona se llama " ++ nombre ++ ", su edad es " ++ show edad ++ "y su sexo es " ++ [sexo]
-- infoCompletaSerVivo (Perro nombre edad _) = "Este perrito se llama " ++ nombre ++ " y tiene " ++ show edad ++ " años"

-- instance Eq Persona where 
--     (==) (Pers nombre1 edad1) (Pers nombre2 edad2) = nombre1 == nombre2 && edad1 == edad2

instance Eq Persona where 
     (==) p1 p2 = (nombre p1) == (nombre p2) && (edad p1) == (edad p2)

instance Num Persona where 
    (+) (Pers nombre1 edad1) (Pers nombre2 edad2) = Pers (nombre1 ++ "+" ++  nombre2) (edad1 + edad2)
    (*) (Pers nombre1 edad1) (Pers nombre2 edad2) = Pers (nombre1 ++ "*" ++  nombre2) (edad1 * edad2)

instance Ord Persona where 
    (<) p1 p2 =  (edad p1) <= (edad p2) && (edad p1) /= (edad p2)
    (>) (Pers _ edad1) (Pers _ edad2) =  edad1 >= edad2 && edad1 /= edad2
    (<=) (Pers _ edad1) (Pers _ edad2) =  edad1 <= edad2 
    (>=) (Pers _ edad1) (Pers _ edad2) =  edad1 >= edad2 
    --max p1 p2 = if (edad p1 >= edad p2) then (edad p1) else (edad p2)

data CoordType = Coord Float Float deriving Show

c1 = Coord 14.0 (-2.0)
--c1 = Coord 14.0 2.0
c2 = Coord 27.0 8.0
c3 = Coord 78.0 2.7

c4 = [c1,c2,c3]

getX (Coord x _ ) = x
getY (Coord _ y ) = y

firstQuad [] = True
firstQuad ( (Coord x y) : cs ) = (x >= 0) && (y >= 0) && (firstQuad cs)

firstQuad' [] = True
firstQuad' (e:l) = ((getX e) >= 0) && ((getY e) >= 0) && (firstQuad l)

firstQuad'' [] = True
firstQuad'' (e:l) = if (getX e) < 0 || (getY e) < 0 then False else firstQuad l

data Vector a = Vector a a a deriving (Show)

data MiArbol a = MiNodoVacio 
                | MiNodoLleno a (MiArbol a) (MiArbol a)
                deriving (Show)

arbol1 = MiNodoLleno 5 (MiNodoLleno 3 (MiNodoLleno 4 MiNodoVacio MiNodoVacio) MiNodoVacio) (MiNodoLleno 2 MiNodoVacio MiNodoVacio)

