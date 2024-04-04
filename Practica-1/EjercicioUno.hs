module EjercicioUno where
--A
five:: Int
five = 5


--B
doble:: Int -> Int
doble x = 2*x

appply :: (Int -> Int) -> Int -> Int
appply f y = f y + 1

apply' :: Int -> Int -> Int -> Int 
apply' f x = f x + 1

--C
identidad x = x


--D
first (x,y) = x


--E
--que carajo la derivada en haskell


--F
signo x = if x > 0 then '+' else (if x < 0 then '-' else '0') 


--G
absoluto x = if (signo x) == '+' then x else x*(-1)


--H
potencia a b = product (replicate a b)


--I
xorr x y = if x /= y then True else False


--J
max3 x y z = max (max x y) z


--K
swap (a,b) = (b,a)