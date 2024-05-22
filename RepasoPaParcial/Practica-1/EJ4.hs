module EJ4 where

a1 = (||) False
a3 = (False ||)
a3:: Bool -> Bool 
--a)
foo1 :: Bool -> (Bool -> Bool) 
foo1 p = if p then (p &&) else (p &&)

--b)
-- (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
foo2 :: (b  ->  c) -> (a  ->  b) ->  a ->  c
foo2 x y z = x (y z)

--c)
-- (t1 -> t2 -> t3) -> t1 -> t2 -> t3
foo3 :: (a  -> b  ->  c) ->  a ->  b ->  c
foo3 x y z = x y z

--d)
-- (t -> a) -> t -> [a] -> [a]
foo4 :: (a -> b) -> a -> [b] -> [b]
foo4 x y z = x y : z

--e)
-- a -> (t -> [a]) -> t -> [a]
foo5 ::  b -> (a -> [b]) -> a -> [b]
foo5 x y z = x : y z

--f)
-- [a] -> (t -> [a]) -> t -> [a]
foo6 :: [b] -> (a -> [b]) -> a -> [b]
foo6 x y z = x ++ y z 

--g)
-- [[a]] -> ([[a]] -> Bool) -> [a]
foo7 :: [[a]] -> ([[a]] -> Bool) -> [a]
foo7 a b = if b a then head a else []

--h)
foo8 :: [a] -> ([a] -> Bool) -> [a]
foo8 a b = if b a then a else []

--i)
foo9 a b = if b a then head (:a) else (:[])
