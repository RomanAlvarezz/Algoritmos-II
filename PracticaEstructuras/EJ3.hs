module EJ3 where

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

cl1 = Consnoc 1 (CUnit 2) 3
cl2 = Consnoc 12 (Consnoc 34 EmptyCL 99) 4
--A)
headCL :: CList a -> a
-- headCL (CUnit x) = CUnit x
-- headCL (Consnoc x cl y) = CUnit x
headCL (CUnit x) = x
headCL (Consnoc x cl y) = x

lastCL (CUnit x) = x
lastCL (Consnoc x cl y) = y

tailCL :: CList a -> CList a
tailCL (CUnit x) = EmptyCL
--tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x cl y) = Consnoc (headCL cl) (tailCL cl) y

--Borra el Ultimo
--borrarUCL (EmptyCL) = EmptyCL creo q no hace falta esto
borrarUCL (CUnit x) = EmptyCL
borrarUCL (Consnoc x EmptyCL y) = CUnit x
borrarUCL (Consnoc x cl y) = Consnoc x (borrarUCL cl) (lastCL cl)

isEmptyCL (EmptyCL) = True 
isEmptyCL _ = False

isCUnit (CUnit x) = True 
isCUnit _ = False 

--Agrega al principio
cons e (EmptyCL) = CUnit e 
cons e (CUnit x) = Consnoc e EmptyCL x
cons e (Consnoc x cl y) = Consnoc e (cons x cl) y

--Agrega al final
snoc e (EmptyCL) = CUnit e 
snoc e (CUnit x) = Consnoc x EmptyCL e
snoc e (Consnoc x cs y) = Consnoc x (snoc y cs) e

--B)
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x EmptyCL y) = Consnoc y EmptyCL x
reverseCL (Consnoc x cl y) = Consnoc y (reverseCL cl) x

--C)

borrarUltimo (e:[]) = []
borrarUltimo (e:l) = e:(borrarUltimo l)
--initss [1,2,3] --> [[],[1],[1,2],[1,2,3]]
initss' [] = []
initss' xs = initss' (borrarUltimo xs) ++ [xs]
initss xs = []:initss' xs
--lastss [1,2,3] --> [[],[3],[2,3],[1,2,3]]
lasts' [] = []
lasts' l = lasts' (borrarUltimo l) ++ [ reverse l ]
lasts l = []:lasts' (reverse l) 

--initsCL Consnoc 1 (CUnit 2) 3 --> Que devuelve????

initCL'  = 
initCL cl = snoc EmptyCL (initCL' (borrarUCL cl) ) cl

