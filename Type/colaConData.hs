 module Cola (Cola, colaVacia, enColar, desenColar, colaEstaVacia, frente) where

    data Cola a = Vacio | Col a (Cola a) deriving (Show,Eq)

    colaVacia = Vacio 

    enColar el Vacio = Col el Vacio 
    enColar el c = Col el c

    desenColar (Col el Vacio) = Vacio
    desenColar (Col el c) = Col el (desenColar c) 

    frente Vacio = error "La cola esta vacia"
    frente (Col el Vacio) = el
    frente (Col el c) = el

    colaEstaVacia Vacio = True
    colaEstaVacia _ = False

    colaAlReves :: Cola a -> [a]
    colaAlReves (Col el Vacio) = [el]
    colaAlReves (Col el c) = (el):(colaAlReves c) 

    mostrarCola Vacio = []
    mostrarCola c = reverse (colaAlReves c)

