module LineaAlternativo where 
--Este seria un ejercicio alternativo al de Linea (esto no esta en la practica lo hicimos en clase)
type Cadena = [Char]
type Linea = (Cadena,Cadena)

ej1 = ("oH", "la Mundo")

insertar c (ls,rs) = (c:ls,rs)

movIzq ([],rs) = ([],rs)
movIzq (e:ls,rs) = (ls,e:rs)

movDer (ls,[]) = (ls,[])
movDer (ls,e:rs) = (e:ls,rs)

movIni (ls,rs) = ([], reverse ls ++ rs)

movFin (ls,rs) = (reverse rs ++ ls, [])

