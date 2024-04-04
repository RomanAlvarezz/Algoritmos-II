module EjercicioSeis where
import Data.Char

--A
sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (e:l) = e + 2
{-
                  e + e + sumaLista l
                  e + e + e + sumaLista l...
-}

--B
alguno :: [Bool] -> Bool
alguno [] = False
alguno (e:l) = if e then True else alguno l 

--C
todos :: [Bool] -> Bool
todos [] = True
todos (e:l) = if not e then False else todos l 


--D (preguntar al profe)
--codes:: [Char] --> [Int]


--E
--restos :: Num a => [a] -> Int -> [a]
restos [] d = []
restos (e:l) d = (mod e d):(restos l d)


--F
cuadrados:: [Int] -> [Int]
cuadrados [] = []
cuadrados (e:l) = (e*e):(cuadrados l)


--G
longitudes [] = []
longitudes (e:l) = (length e):(longitudes l)


--H  (preguntar al profe)
-- primerElementoEnTupla (x,_) = x
-- segundoElementoEnTupla (_,x) = x
primerElementoEnTupla (x,y) = x
segundoElementoEnTupla (x,y) = y
orden :: [(Int,Int)] -> [(Int,Int)]
orden [] = []
--orden (e:l) = if (primerElementoEnTupla e) < (segundoElementoEnTupla e) then e:(orden l) else (orden l)
orden (e:l) = if (primerElementoEnTupla e) < (segundoElementoEnTupla e)*3
                  then e:(orden l) 
                  else (orden l)

listaDeTuplas = [(1,2),(4,3),(70,90),(1000,6)]


--I
pares :: [Int] -> [Int]
pares [] = []
pares (e:l) = if (e `mod` 2 == 0) then e:(pares l) else (pares l)


--J
letras :: [Char] -> [Char]
letras [] = []
letras (e:l) = if (((ord e) >= 65 && (ord e) <= 90) || ((ord e) >= 97 && (ord e) <= 122)) then e:(letras l) else (letras l)

--K
--masDe :: [a] -> [a]
masDe [] n = []
masDe (e:xss) n = if ((length e) > n) then e:(masDe xss n) else (masDe xss n)


