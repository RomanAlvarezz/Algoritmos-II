data Node a = Nod a (Node a) (Node a) deriving Show 

inTree x EmptyNode = False 
inTree x (Nod e lt rt) | x == e = True 
                       | x < e = inTree x lt 
                       | x > e = inTree x rt

addTree x EmptyNode = Nod x EmptyNode EmptyNode
addTree x (Nod e lt rt) | x == e = (Nod e lt rt)
                        | x < e = Nod e (addTree x lt) rt
                        | x > e = Nod e lt (addTree x rt)

minTree (Nod e EmptyNode rt) = (e, rt)
minTree (Nod e lt rt) = let 
                            (x, new_lt) = minTree lt 
                        in 
                            (x, Nod e new_lt rt)

delTree x EmptyNode = EmptyNode 
delTree x (Nod e EmptyNode rt) | x == e = rt 
delTree x (Nod e lt EmptyNode) | x == e = lt 
delTree x (Nod e lt rt) | x < e = Nod e (delTree x lt) rt
                        | x > e = Nod e lt (delTree rt) 
                        | x == e = let 
                                      (a, new_rt) = minTree rt
                                   in 
                                     (Nod a lt new_rt)

data Dict a = Dic (Node a) deriving Show 

mkDict = Dic (EmptyNode) 

insertDict x (Dic t) =  Dict (addTree x t)

inDict x (Dic t) = inTree x t 

delDict x (Dic t) = Dict (delTree x t) 

delDictPrior x (Dic t) = let (e,new_t) = minTree t in (Dic new_t) 

minDictElement x (Dic t) = let (e,new_t) = minTree t in e 


preOrder EmptyNode = []
preOrder (Nod x lt rt) = [x] ++ (preOrder lt ++ preOrder rt)

inOrder EmptyNode = []
inOrder (Nod x lt rt) = inOrder lt ++ [x] ++ inOrder rt

postOrder EmptyNode = [] 
postOrder (Nod x lt rt) = postOrder lt ++ postOrder rt ++ [x]


-- Crea un nuevo árbol a partir de una lista
arbLista EmptyNode = []
arbLista (e:l) = Nod e (arbLista [x | x <- l, x < e]) (arbLista [x | x <- l, x >= e]) 

arbNodos EmptyNode = 0
arbNodos (Nod e lt rt) = 1 + arbNodos lt + arbNodos rt 

arbHojas EmptyNode = 0
arbHojas (Nod e EmptyNode EmptyNode) = 1 
arbHojas (Nod e lt rt) = arbHojas lt + arbHojas rt

alturaArb EmptyNode = 0
alturaArb (Nod e lt rt) = 1 + max (alturaArb lt) (alturaArb rt)

data Set a = S [a] deriving Show 

emptySet = S []

setEmpty (S []) = True 
setEmpty _ = False 

inSet x (Set []) = False 
inSet x (Set (e:l)) = if x == e then True else inSet x (Set l)

addSet x (Set l) = if (inSet x (Set l)) then Set l else Set (x:l) 

delSet x (Set []) = Set [] 
delSet x (Set (e:l)) = if x /= e then addSet e (delSet x (Set l)) else delSet x Set l 

unionSet (Set []) (Set [x])= Set [x] 
unionSet (Set (x:xs)) (Set ys) = if inSet x (Set ys) then unionSet (Set xs) (Set ys) else unionSet (Set xs) (Set (x:ys))  
 