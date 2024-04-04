module EJ6 where 

--a)
suma [] = 0
suma (e:l) = e + suma l

--b)

alguno [] = False
alguno (e:l) = if e then True else alguno l

--c)
todos [] = True
todos (e:l) = if e then todos l else False

--d)

--e)
restos [] _ = []
restos (e:l) n = (e `mod` n):(restos l n)

--f)
cuadrados [] = []
cuadrados (e:l) = (e*e):(cuadrados l)
cuadrados' l = [x*x | x <- l]

--g)
longitudes [] = 0
longitudes (l:ls) = length l + longitudes ls 

--h)
orden [] = []
orden ((x,y):l) = if x < 3*y then (x,y):(orden l) else orden l 

--i)
pares [] = []
pares (e:l) = if even e then e:(pares l) else pares l

--j)


--k)
masDe [] _ = []
masDe (l:ls) n = if length l > n then l:(masDe ls n) else masDe ls n
