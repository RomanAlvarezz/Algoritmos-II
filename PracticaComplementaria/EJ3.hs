module EJ3 where
--data Maybe a = Nothing | Just a
--data Estado a = Est Nombre [a] deriving Show
type Nombre = [Char]
data Estado a = Est [(Nombre, a)] deriving Show

e1 = Est [("n1",1),("n2",3),("n4",89)]

inicial :: Estado a 
inicial = Est [] 

update :: Nombre -> a -> Estado a -> Estado a 
update nom x (Est []) = Est [(nom,x)]
update nom x (Est xs) = Est (map (\ (n,y) -> if n == nom then (n,x) else (n,y)) xs)

-- update "n2" 45 e1

lookfor :: Nombre -> Estado a -> Maybe a 
lookfor nom (Est []) = Nothing
lookfor nom (Est ((n,x):xs)) | n == nom = Just x 
                             | otherwise = lookfor nom (Est xs)

free :: Nombre -> Estado a -> Estado a 
free nom (Est []) = Est [] 
free nom (Est xs) = Est (filter (\ (n,x) -> n /= nom) xs)