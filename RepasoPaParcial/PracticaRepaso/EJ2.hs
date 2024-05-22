module EJ2 where 

--a) (Int -> Int) -> Int
doble :: Int -> Int
doble x = 2 * x 
dobleMas5 :: (Int -> Int) -> Int
dobleMas5 f  = f 2 + 5

--b) Int -> (Int -> Int)
suma::Int -> (Int -> Int)
suma x = doble 

-- maxxx = maxx 3

--c) (Int -> Int) -> (Int -> Int)
mayorQue3 :: Int -> Int 
mayorQue3 x = if x > 3 then x else 3

funcionC :: (Int -> Int) -> (Int -> Int)
funcionC mayorQue3 = mayorQue3

--d)  Int -> Bool
funcionD :: Int -> Bool
funcionD x = True

--e) Bool -> (Bool -> Bool)
funcionE' :: Bool -> Bool 
funcionE' a = a && True 
funcionE :: Bool -> (Bool -> Bool)
funcionE x = funcionE' 

--f) (Int,Char) -> Bool 
funcionF :: (Int,Char) -> Bool 
funcionF (x,c) = (c == 'a' && x > 10) 

--g) (Int,Int) -> Int 
funcionG :: (Int,Int) -> Int 
funcionG (x,y) = x + y

--h) Int -> (Int,Int)
funcionH :: Int -> (Int,Int)
funcionH x = (x,x)

--i) a -> Bool
funcionI x = True 

--j) a -> a 
funcionJ x = x
