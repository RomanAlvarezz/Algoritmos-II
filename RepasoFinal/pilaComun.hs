data Pila a = Vacio | Pil a (Pila a) deriving Show

push x p = Pil x p 

pop Vacio = error "Pila VACIA"
pop (Pil x p) = p 

top Vacio = error "Pila VACIA"
top (Pil x p) = x 

pilaEstaVacia Vacio = True 
pilaEstaVacia _ = False 

pilaVacia = Vacio


newtype Stack a = Stk [a] deriving Show

pushS x (Stk l) = Stk (x:l)

popS (Stk []) = error "Stack VACIO"
popS (Stk (e:l)) = Stk l 

topS (Stk []) = error "Stack VACIO"
topS (Stk (e:l)) = e

pilaEstaVaciaS (Stk []) = True 
pilaEstaVaciaS _ = False 

pilaVaciaS = Stk []



