data Color = R | B deriving Show 
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show 

a1 = insert 55 E 
a2 = insert 78 a1
a3 = insert 26 a2  

b1 = insert 55 E 
b2 = insert 78 b1
b3 = insert 26 b2  

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

insert' :: Ord a => a -> RBT a -> RBT a 
insert' x t = makeBlack (ins' x t)

ins' x E = T R E x E
ins' x (T c l y r) | x < y =  lbalance c (ins' x l) y r 
                   | x > y = rbalance c l y (ins' x r)
                   | otherwise = T c l y r 

makeBlack E = E 
makeBlack (T _ l x r) = T B l x r  

balance :: Color -> RBT a -> a -> RBT a -> RBT a 
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

lbalance :: Color -> RBT a -> a -> RBT a -> RBT a 
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c l a r = T c l a r

rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l a r = T c l a r