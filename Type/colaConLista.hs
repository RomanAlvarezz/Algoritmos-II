 module Cola (Queue, emptyQueue, enQueue, deQueue, queueIsEmpty, front) where

    typedef Queue a = Q [a] deriving (Show)

    emptyQueue = Q []

    enQueue x (Q s) = Q (s ++ [x])

    deQueue (Q[]) = error "Cola Vacia"
    deQueue (Q (x:t)) = t 

    front (Q []) = error "Cola Vacia"
    front (Q(x:xs)) = x 

    queueIsEmpty (Q[]) = True 
    queueIsEmpty (Q _ ) = False


