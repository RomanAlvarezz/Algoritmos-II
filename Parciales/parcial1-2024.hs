module Parcial where

m1 :: [(Int, [Char])]
m1 = [(3,"Hola"),(5,"como"),(10,"va")]

enOrdenASC :: [(Int, String)] -> Bool
enOrdenASC xs = let os = map (\(i, _) -> i) xs 
--enOrdenASC xs = let os = map getIndices xs  <-- no le tenia q pasar getIndices la concha de mi madre
                    ady = zip os (tail os) 
                in foldr checkeo True ady

checkeo :: (Int, Int) -> Bool -> Bool
checkeo (x, y) bool = bool && x < y

getIndices [] = []
getIndices ((i,_):xs) = i : getIndices xs 

faltantes :: [(Int, [Char])] -> [Int]
faltantes xs = concat [ valoresEntre i1 i2 | ((i1,_),(i2,_)) <- zip xs (tail xs) ]

valoresEntre x y | (x + 1) == y = []
                 | (x + 2) == y = [(x + 1)]
                 | otherwise = [z | z <- [(x+1)..(y-1)]]

caniaconruda f g h =  f (g h) (g h)
--caniaconruda:: ( b -> b -> c )-> (a -> b) -> a -> c