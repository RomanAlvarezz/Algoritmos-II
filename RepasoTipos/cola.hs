data Cola a = Vacio | Col a (Cola a) deriving Show 

--Aca la cola la pense al reves de como esta echa en Type/colaConData.hs
enColar y Vacio = Cola y Vacio
enColar y (Cola x xs) = Cola x (enColar y xs)

desenColar Vacio = Vacio 
desenColar (Col x s) = Col s  