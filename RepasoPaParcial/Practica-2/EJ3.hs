module EJ3 where

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

cl1 = Consnoc 1 EmptyCL 3
cl2 = Consnoc 12 (Consnoc 34 EmptyCL 99) 4
cl3 = Consnoc 1 (CUnit 34) 3
--cl3:: CList (CList a)
--cl3 = Consnoc (Consnoc 1 EmptyCL 2) (Consnoc 1 EmptyCL 2) (Consnoc 1 EmptyCL 2)

headCL (CUnit x) = x 
headCL (Consnoc x xs y) = x 

tailCL (CUnit x) = EmptyCL 
tailCL (Consnoc x xs y) = Consnoc (headCL xs) (tailCL xs) y

isEmptyCL EmptyCL = True 
isEmptyCL _ = False 

isCUnit (CUnit x) = True 
isCUnit _ = False 

ultimoCL (CUnit x) = x
ultimoCL (Consnoc x xs y) = y

borrarUCL EmptyCL = EmptyCL
borrarUCL (CUnit x) = EmptyCL
borrarUCL (Consnoc a EmptyCL b) = CUnit a 
borrarUCL (Consnoc a cl b) = Consnoc a (borrarUCL cl) (ultimoCL cl)

borroPCL EmptyCL = EmptyCL
borroPCL (CUnit x) = EmptyCL
borroPCL (Consnoc a EmptyCL b) = CUnit b 
borroPCL (Consnoc a cl b) = Consnoc (headCL cl) (borroPCL cl) b
--initsCL :: CList a -> CList (CList a)
initsCL EmptyCL = EmptyCL
initsCL (CUnit x) = CUnit (CUnit x)
initsCL xs = snoc (initsCL (borrarUCL xs)) xs

--Agrega un elemento al principio
cons a EmptyCL = CUnit a
cons a (CUnit x) = Consnoc a EmptyCL x
cons a (Consnoc x xs y) = Consnoc a (cons x xs) y

--Agrega un elemento al final
snoc EmptyCL a = CUnit a
snoc (CUnit x) a = Consnoc x EmptyCL a
snoc (Consnoc x xs y) a = Consnoc x (snoc xs y) a


lastCL EmptyCL = EmptyCL
lastCL (CUnit x) = CUnit (CUnit x)
lastCL xs = cons xs (lastCL (borroPCL xs)) 


borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs
serie' [] = [[]] 
serie' xs = serie' (borrarUltimo xs) ++ [xs] 
serie xs = serie' xs

{-- 
--Agrega un elemento al principio
cons a EmptyCL = CUnit a
cons a (CUnit x) = Consnoc a EmptyCL x
cons a (Consnoc x xs y) = Consnoc a (cons x xs) y

--Agrega un elemento al final
snoc EmptyCL a = CUnit a
snoc (CUnit x) a = Consnoc x EmptyCL a
snoc (Consnoc x xs y) a = Consnoc x (snoc xs y) a

concatCL x EmptyCL = x
concatCL EmptyCL y = y
concatCL x y       = concatCL (borrarUltimoCL x) (cons (ultimoCL x) y)

--initsCL :: CList a -> CList (CList a)
initsCL' EmptyCL = EmptyCL
initsCL' (CUnit x) = CUnit (CUnit x)
initsCL' xs = snoc (initsCL' (borrarUltimoCL xs)) xs

initsCL EmptyCL = CUnit EmptyCL
initsCL xs = cons EmptyCL (initsCL' xs)

lastCL' EmptyCL = EmptyCL
lastCL' (CUnit x) = CUnit (CUnit x)
lastCL' xs = snoc (lastCL' (tailCL xs)) xs

lastCL EmptyCL = CUnit EmptyCL
lastCL xs = cons EmptyCL (lastCL' xs)
--}