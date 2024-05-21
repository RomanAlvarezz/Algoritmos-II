type Rank = Int 
data Heap a = E | N Rank a (Heap a) (Heap a)

merge :: Ord a => Heap a -> Heap a -> Heap a 
merge h1 E = h1 
merge E h2 = h2 
merge h1@(N _ x a1 b1) h2@(N _ x a2 b2) = if x <= y then makeH x a1 (merge b1 h2)
                                                    else makeH y a2 (merge b2 h1)

rank :: Heap a -> Rank 
rank E = 0 
rank (N r _ _ _ ) = r 

makeH x a b = if rank a >= rank b then N (rank b + 1) x a b 
                                  else N (rank a + 1) x b a

insert :: Ord a => a -> Heap a -> Heap a 
insert x h = merge (N 1 x E E) h 

findMin :: Ord a => Heap a -> a 
findMin (N x a b) = x 

deleteMin :: Ord => Heap a -> Heap a 
deleteMin (N x a b) = merge a b 