module Prueba2 where
--data Bool = True | False
data Answer = Yes | No | Unknown deriving Show

flipp :: Answer -> Answer
flipp Yes = No
flipp No = Yes
flipp Unknown = Unknown

--sum types
data SmallNum = One | Two | Three | Four | Five deriving Show

--product Types
data Coordinates = Coor SmallNum SmallNum deriving Show

-- type Nombre = [Char]
-- type Edad = Int
-- type Dni = Int 

-- data Persona = Pers Nombre Edad Dni deriving Show

add1 :: SmallNum -> Int
add1 One = 1
add1 Two = 2
add1 Three = 3
add1 Four = 4
add1 Five = 5

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show
-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
 
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

desplazar :: Shape -> Float -> Float -> Shape
desplazar (Circle (Point cx cy) r) x y = (Circle (Point (cx + x) (cy + y)) r)
desplazar (Rectangle (Point upx upy) (Point downx downy)) x y = (Rectangle (Point (upx + x) (upy + y)) (Point (downx + x) (downy + y)))

-- data Persona1 = Person1 { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      , phoneNumber :: String
--                      , flavor :: String
--                      } deriving (Show)

data Persona1 = Pers Nombre Edad deriving Show

type Persona2 = ([Char],Int)

type Nombre = [Char]
type Edad = Int

newtype Persona3 = Person3 (Nombre, Edad) deriving Show
esJoven (Person3 (_,n)) = n < 18 