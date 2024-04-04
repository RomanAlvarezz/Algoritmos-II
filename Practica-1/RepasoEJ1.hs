module EJ1 where 

--a)
five _ = 5

--b)
aaply f x = f x

--c)
identidad x = x

--d)
first (x,_) = x

--e)

--f)
signo x | x < 0 = -1
        | x > 0 = 1
        | otherwise = 0

--g)
absoluto x = if signo x >= 0 then x else -x

--h)
{--
potencia _ 0 = 1
potencia base exp = base * potencia base (exp-1)
--}

pot :: Integer -> Integer -> Integer
pot _ 0 = 1
pot base exp = base * pot base (exp - 1)

--i)


--j)
max3 x y z = max (max x y) z

--k)
swap (x,y) = (y,x)