module EJ3 where

--a) (Int -> Int) -> (Bool -> Bool)
five :: Int -> Int 
five x = x

reverseTechnique :: Bool -> Bool 
reverseTechnique True = False
reverseTechnique False = True

funcionA :: (Int -> Int) -> (Bool -> Bool)
funcionA f = reverseTechnique

--b) Bool -> (Int -> Bool)
esPositivo :: (Int -> Bool)
esPositivo x = x > 0 
funcionB :: Bool -> (Int -> Bool)
funcionB b = esPositivo

--c) Char -> Char
funcionC :: Char -> Char
funcionC c = c

--d) Int -> (Int -> Bool) -> [Int]
funcionD :: Int -> (Int -> Bool) -> [Int]
funcionD (-3) _ = [(-3)]
funcionD x f = if (f x) == True then x : (funcionD (x - 1) f) else (funcionD (x - 1) f)

--e) [a] -> (a -> [b]) -> [b]
suma1 x = [ x + 1 ]
funcionE :: [a] -> (a -> [b]) -> [b]
funcionE [] _ = [] 
funcionE (x:xs) f = f x ++ (funcionE xs f) 

--f) [[a]] -> (a -> Bool) -> [a]
primeroPositivo [] = False 
primeroPositivo (x:xs) = x > 0
--funcionF :: [[a]] -> (a -> Bool) -> [a]
funcionF [] _ = []
funcionF (x:xs) f = if (f x) then x ++ (funcionF xs f) else (funcionF xs f)

--g) (a,b,c) -> Bool
funcionG (x,y,z) = True

--h) (a,b,c) -> Int -> c
funcionH :: (a,b,c) -> Int -> c
funcionH (x,y,z) n = if n + 1 == 2 then z else z

--i) (a,a,a) -> Int -> a
funcionI :: (a,a,a) -> Int -> a
funcionI (x1,x2,x3) n = if n + 1 == 2 then x1 else x2

