module EJ2 where

--A)
--cambiosAux::[(Int,Int)] -> [Int]
cambiosAux ((x,xpos):(y,ypos):[]) = if x /= y then [xpos]
                                             else []
cambiosAux ((x,xpos):(y,ypos):l) = if x /= y then xpos:(cambiosAux ((y,ypos):l))
                                             else cambiosAux ((y,ypos):l)
cambios [] = []
cambios l = cambiosAux (zip l [0..]) -- VERSION sin listas por comprension

--cambios' l = [(x, xpos) | (x, xpos) <- zip l [0..], x == xpos]
cambios' l = [xpos | (x, xpos) <- zip l [0..], (y, ypos) <- zip l [0..], ypos == xpos + 1 && x /= y] --Version con listas por comprension

--cambios l = [ pos | (e,pos) <- zip l [0..], ]

--testeo::[(Int,Int)] -> [(Int,Int)]
--testeo l = l
--testeo l = [ e | e <- l]


--prueba'::(Eq a) => a -> a
--prueba' l = l




--B)
oblongoAux (x:y:[]) = [(x*y)]
oblongoAux (x:y:l) = (x*y):(oblongoAux (y:l))
oblongo 0 = error "El limite debe ser mayor a 0"
oblongo n = oblongoAux ([1..n])

oblongoAux' (x:y:l) 0 = [(x*y)]
oblongoAux' (x:y:l) i = (x*y):(oblongoAux' (y:l) (i-1))
oblongo' 0 = error "El limite debe ser mayor a 0"
oblongo' n = oblongoAux' [1..] (n-1)

oblongo'' limite = [x*y | (x, xpos) <- zip [1..limite] [0..], (y,ypos) <- zip [1..limite] [0..], xpos + 1 == ypos]
--(x, xpos) <- zip l [0..], (y, ypos) <- zip l [0..]




--C)
divisores' x | x == 0 = error "Infinito"
             | x > 0 = [n | n <- [1..(x-1)], x `mod` n == 0] 
             | x < 0 = [n | n <- [1..((x+1)*(-1))], x `mod` n == 0] 
divisores x = [n | n <- [1..(x-1)], x `mod` n == 0]

abundantes = [x | x <- [1..], x < sum (divisores x)]




--D) 

repitoLetra _ 0 = []
repitoLetra letra n = letra:(repitoLetra letra (n-1))

tomopalabra cadena = concat (map (\ (letra,pos) -> repitoLetra letra pos) (zip cadena [1..]))

eco cadena = concat [ repitoLetra letra pos | (letra,pos) <- zip cadena [1..] ]




--E)
euler n = sum [x | x <- [1..(n-1)], x `mod` 3 == 0 || x `mod` 5 == 0] 


--random)
multiplos n = [x | x <- [1..], x `mod` n == 0]
multiplos' _ 0 = []
multiplos' cantidadDeMultiplos n = multiplos' n (cantidadDeMultiplos - 1) ++ [cantidadDeMultiplos * n]





--F)
expandirAux _ 0 = []
expandirAux n c = n:(expandirAux n (c-1))

expandirAux' n = replicate n n

expandir l = concat [ expandirAux' x | x <- l]

expandir' l = foldr (++) [] [ expandirAux' x | x <- l]

--tomopalabra cadena = concat (map (\ (letra,pos) -> repitoLetra letra pos) (zip cadena [1..]))

