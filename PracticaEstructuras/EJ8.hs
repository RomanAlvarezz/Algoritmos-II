module EJ8 where

data Color = R | B deriving (Eq,Show) 
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show 

a1 = insert 55 E 
a2 = insert 78 a1
a3 = insert 26 a2  -- fromOrdList [26,78,55] me tiene que dar igual que a3

memberRBT :: Ord a => a -> RBT a -> Bool 
memberRBT a E = False 
memberRBT a (T _ l b r) | a == b = True 
                        | a < b =  memberRBT a l
                        | a > b =  memberRBT a r 

insert :: Ord a => a -> RBT a -> RBT a 
insert x t = makeBlack (ins x t)
    where ins x E = T R E x E 
          ins x (T c l y r) | x < y =  balance c (ins x l) y r 
                            | x > y = balance c l y (ins x r)
                            | otherwise = T c l y r 

makeBlack E = E 
makeBlack (T _ l x r) = T B l x r  

balance :: Color -> RBT a -> a -> RBT a -> RBT a 
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

--8)

--fromOrdList :: Ord a => [a] -> RBT a 
-- fromOrdList [x] = insert x E
-- fromOrdList (x:l) = insert x (fromOrdList l)

fromOrdList xs = makeBlack (if (even (truncate (logBase 2 (fromIntegral (length xs))))) then
                 fromOrdList' xs R else fromOrdList' xs B)
                        
fromOrdList' [] c = E 
fromOrdList' xs c = let n = div (length xs) 2 
                        x = xs !! n 
                        ant = take n xs 
                        pos = drop (n+1) xs 
                        c' = if c == R then B else R 
                    in T c (fromOrdList' ant c') x (fromOrdList' pos c')