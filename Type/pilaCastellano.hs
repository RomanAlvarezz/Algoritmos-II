module Pilaa (Pila, pop, push, top, pilaVacia, pilaEstaVacia, mostrarPila) where

    data Pila a = Vacio | Pil a (Pila a) deriving (Eq,Show)

    pilaVacia = Vacio

    push e p = Pil e p

    pop Vacio = error "La pila esta vacia!"
    pop (Pil _ p) = p 

    top:: Pila a -> a
    top Vacio = error "La pila esta vacia!"
    top (Pil e s) = e 

    pilaEstaVacia :: Pila a -> Bool
    pilaEstaVacia Vacio = True 
    pilaEstaVacia _ = False

    mostrarPila Vacio = "-"
    mostrarPila (Pil e p) =  show e ++ "|" ++ mostrarPila p
    

