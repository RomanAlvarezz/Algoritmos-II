data Queue a = Q [a] deriving Show 

insertOrdered x [] = [x]
insertOrdered x (e:l) = if x < e then x:e:l else e:(insertOrdered x l)

emptyQueue = Q []

enQueue x (Q l) = Q (l ++ x)

deQueue (Q []) = error "Empty Queue"
deQueue (Q (x:l)) = Q l 

front (Q []) = error "Empty Queue"
deQueue (Q (x:l)) = x 

queueIsEmpty (Q []) = True
queueIsEmpty _ = False