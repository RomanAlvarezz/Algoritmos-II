module EJ5 where

--a)
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

--b)
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x:acc else acc) []

--c)
unzip' :: [(a,b)] -> ([a],[b])
unzip' xs = foldr (\(x,y) (a,b) -> (x:a,y:b)) ([],[]) xs

--d)
pair2List :: (a,[b]) -> [(a,b)]
pair2List (y,ys) = foldr (\ x acc -> (y,x):acc) [] ys

--e)
maxL :: (Int,Int) -> (Int,Int) -> (Int,Int) 
maxL (x,y) (z,k) = if (y - x) >= (k - z) then (x,y) else (z,k)

maxSeg :: [(Int,Int)] -> (Int,Int)
maxSeg xs = foldr maxL (0,0) xs