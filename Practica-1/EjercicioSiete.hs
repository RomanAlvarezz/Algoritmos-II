module EjercicioSiete where

-- scalarproduct :: [Int] -> [Int] -> Int -> Int 
-- scalarproduct [] [] = []
-- scalarproduct (e1:l1) (e2:l2) = (e1*e2):(scalarproduct l1 l2)  

-- scalarproduct = [(x,y) | x <- [1,4,5], y <- [3,7,2]]

scalarproduct l1 l2 = sum [x*y | (x,y) <- (zipp l1 l2)]



zipp [] [] = []
zipp [] _ = []
zipp _ [] = []
zipp (e1:l1) (e2:l2) = (e1,e2):(zipp l1 l2)  