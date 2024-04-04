data Cola a = Col a (Cola a) | Vacio deriving Show 

colaVacia = Vacio 

enColar x c = Col x c 

desenColar Vacio = error "Cola Vacia" 
desenColar (Col x Vacio) = Vacio
desenColar (Col x c) = Col x (desenColar c)

frente Vacio = error "Cola Vacia"  
frente (Col x Vacio) = x
frente (Col x c) = frente c

colaEstaVacia Vacio = True 
colaEstaVacia _ = False