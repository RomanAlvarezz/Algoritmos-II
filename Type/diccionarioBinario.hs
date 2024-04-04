data Arbol a = NodoVacio | Arb a (Arbol a) (Arbol a) deriving (Show)

mkNewTree :: (Ord a) => Arbol a 
inTree :: (Ord a) => a -> Arbol a -> Bool
addTree :: (Ord a) => a -> Arbol a -> Arbol a
delTree :: (Ord a) => a -> Arbol a -> Arbol a
preOrder :: (Ord a) => Arbol a -> [a]
inOrder :: (Ord a) => Arbol a -> [a]
postOrder :: (Ord a) => Arbol a -> [a]

preOrder NodoVacio = []
preOrder (Arb x ai ad) =  x:(preOrder ai ++ preOrder ad)

inOrder NodoVacio = [] 
inOrder (Arb x ai ad) = inOrder ai ++ [x] ++ inOrder ad

postOrder NodoVacio = [] 
postOrder (Arb x ai ad) = postOrder ai ++ postOrder ad ++ [x]


mkNewTree = NodoVacio

inTree x NodoVacio = False 
inTree x (Arb n ai ad) | x == n = True 
                         | x < n = inTree x ai
                         | x > n = inTree x ad  

addTree x NodoVacio = Arb x NodoVacio NodoVacio
addTree x (Arb n ai ad)  | x == n = Arb n ai ad 
                          | x < n = Arb n (addTree x ai) ad 
                          | x > n = Arb n ai (addTree x ad) 

minTree (Arb n NodoVacio ad) = (n, ad)
minTree (Arb n ai ad) = let (x, new_lf) = minTree ai
                        in 
                            (x, Arb n new_lf ad)


delTree x NodoVacio = NodoVacio
delTree x (Arb y ai NodoVacio)  | x == y = ai
delTree x (Arb y NodoVacio ad)  | x == y = ad 
delTree x (Arb y ai ad)  | x < y = Arb y (delTree x ai) ad 
                          | x > y = Arb y ai (delTree x ad)
                          | x == y = let (k,wt) = minTree (ad)
                                     in (Arb k ai wt)


newtype Dict a = Dicc (Arbol a) deriving (Show)

mkNewDict :: (Ord a) => Dict a 
mkNewDict = Dicc (mkNewTree) 

insertDict :: (Ord a) => a -> Dict a -> Dict a
--insertDict NodoVacio (Dicc arbol) = Dicc a 
insertDict x (Dicc arbol) =  Dicc (addTree x arbol)

inDict :: (Ord a) => a -> Dict a -> Bool 
inDict x (Dicc arbol) = inTree x arbol

delDict :: (Ord a) => a -> Dict a -> Dict a 
delDict x (Dicc arbol) = Dicc (delTree x arbol) 


a1 = mkNewTree      
a2 = addTree 20 a1
a3 = addTree 60 a2 
a4 = addTree 30 a3

dic1 = Dicc a4 
dic2 = insertDict 10 dic1