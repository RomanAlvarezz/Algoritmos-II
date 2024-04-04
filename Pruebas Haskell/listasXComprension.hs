module ListasXComprension where

listaDelUnoAlCien = [1..100]

listaDeNumerosParesDelUnoAlCien = [0, 2..100]

listaDeNumerosImparesDelUnoAlCien = [1, 3..20] --Si lo dejo asi se va hasta el infinito

-- Lista por comprensión
multiplosDeTres = [n | n <- [1..30], mod n 3 == 0] -- Multiplos de tres hasta el 30 

notasAlumnos = [("Juan", 10),("Pedro",5),("Tomas", 6),("Jorge",4)]

alumnosAprobados l = [nombre | (nombre, nota) <- l, nota > 5] 

listaConVariasCondiciones = [x | x <- [10..20], x /= 13, x /= 15, x /= 19]

{-}
queremos lista intensional que reemplace cada número impar mayor que diez por “BANG!” y cada número impar menor que diez por “BOOM!”. Si un número no es impar, lo dejamos fuera de la lista.
-}
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

listaDeDosListas = [ (x,y) | x <- [2,5,10], y <- [8,10,11]] -- = [(2,8),(2,10),(2,11),(5,8),(5,10),(5,11),(10,8),(10,10),(10,11)]

length' xs = sum [1 | _ <- xs]

{-
dada una lista de listas de números, vamos eliminar los números impares sin aplanar la lista:
-}

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

numerosPares = [ [ x | x <- xs, even x ] | xs <- xxs]
