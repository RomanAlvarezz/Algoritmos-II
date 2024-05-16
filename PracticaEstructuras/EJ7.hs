module EJ7 where

data Arbol a = Hoja | Nodo (Arbol a) a (Arbol a) deriving Show 

a1 = Nodo (Nodo Hoja 4 Hoja) 10 (Nodo Hoja 90 Hoja)

member'':: (Eq a)=> a -> Arbol a -> Bool 
member'' x Hoja = False 
member'' x (Nodo l n r) = x == n || member'' x l || member'' x r 

minimo :: Arbol a -> a 
minimo (Nodo Hoja n r) = n 
minimo (Nodo l n r) = minimo l 

maximo :: Arbol a -> a 
maximo (Nodo l n Hoja) = n 
maximo (Nodo l n r) = maximo r 

checkBST:: (Ord a) => Arbol a -> Bool -- esta funcion te dice si el arbol es binario o no
--checkBST Hoja = True
checkBST (Nodo Hoja n Hoja) = True
checkBST (Nodo l n Hoja) = maximo l <= n && checkBST l
checkBST (Nodo Hoja n r) = maximo r >= n && checkBST r
checkBST (Nodo l n r) = maximo l <= n && maximo r >= n && checkBST l && checkBST r

insert :: Ord a => a -> Arbol a -> Arbol a 
insert x Hoja = Nodo Hoja x Hoja 
insert x (Nodo l n r) | x <= n = Nodo (insert x l) n r
                      | otherwise = Nodo l n (insert x r)

delete :: (Ord a) => a -> Arbol a -> Arbol a 
delete _ Hoja = Hoja
delete x (Nodo l n r) | x < n = Nodo (delete x l) n r
delete x (Nodo l n r) | x > n = Nodo l n (delete x r)
delete x (Nodo Hoja n Hoja) | x == n = Hoja
delete x (Nodo l n Hoja) | x == n = l
delete x (Nodo Hoja n r) | x == n = r
delete x (Nodo l n r) | x == n = let y = minimo r
                                in Nodo l y (delete y r)


member':: (Ord a) => a -> Arbol a -> Bool 
member' _ Hoja = False 
member' x (Nodo l n r) | x == n = True 
                      | x <= n = member' x l 
                      | x >= n  = member' x r 

--7)
--mem:: a -> Arbol a -> a -> Bool
-- a = elemento a buscar, b = candidato
mem a Hoja b = a == b 
mem a (Nodo l e r) b | a <= b = mem a l e 
                     | otherwise = mem a r e 

member x (Nodo l e r) = mem x (Nodo l e r) e