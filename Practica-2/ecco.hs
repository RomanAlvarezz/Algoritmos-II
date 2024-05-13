module Ecco where 


ecco l = ecco' (zip [1..] l) 
ecco' [] = []
ecco' ((i,l):xs) = letraRepetida i l : ecco' xs

letraRepetida 0 _ = []
letraRepetida n c = c:(letraRepetida (n - 1) c)