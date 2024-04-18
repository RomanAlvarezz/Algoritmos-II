module TYPE  where
--En haskell podemos definir un nuevo nombre para un tipo existente usando una declaracion type
type Par a = (a,a)
mult::Par Int -> Int
mult (x,y) = x*y