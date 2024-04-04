module Pila (Stack, pop, push, top, emptyStack, stackIsEmpty) where
    
    data Stack a = EmptyStack | Stk a (Stack a) deriving (Eq,Show)

    emptyStack = EmptyStack

    push x s = Stk x s

    pop EmptyStack = error "Stack Vacio"
    pop (Stk _ s) = s 

    top EmptyStack = error "Stack Vacio"
    top (Stk x _) = x

    stackIsEmpty EmptyStack = True
    stackIsEmpty _ =  False 

    showStack EmptyStack = "-"
    showStack (Stk x r) = show x ++ "|" ++ showStack r