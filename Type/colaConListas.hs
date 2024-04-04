module ColaConDosListas (
    Cola,
    inserta,
    primero,
    resto,
    esVacia,
    vacia
) where 

data Cola a = C ([a],[a]) deriving Show

escribeCola (C (xs,ys)) = "C " ++ show (xs ++ reverse ys)

vacia :: Cola a 
vacia = C ([],[])

normaliza ([], ys) =  (reverse ys, [])
normaliza p = p

inserta y (C (xs,ys)) =  C (normaliza(xs,y:ys))

primero :: Cola a -> a 
primero (C (x:_,_)) = x 
primero _ = error "primero: cola vacia"

resto :: Cola a -> Cola a
resto (C ([],[])) = error "resto: cola vacia" 
resto (C (_:xs,ys)) = C (normaliza (xs,ys))

esVacia (C (xs,_)) = null xs  

valida (C (xs,ys)) = not (null xs) || null ys


elementos (C (xs,ys)) = xs ++ reverse ys


