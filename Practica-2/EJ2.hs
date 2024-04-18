module EJ2 where

--A)
--cambiosAux::[(Int,Int)] -> [Int]
cambiosAux ((x,xpos):(y,ypos):[]) = if x /= y then [xpos]
                                             else []
cambiosAux ((x,xpos):(y,ypos):l) = if x /= y then xpos:(cambiosAux ((y,ypos):l))
                                             else cambiosAux ((y,ypos):l)
cambios [] = []
cambios l = cambiosAux (zip l [0..]) -- VERSION sin listas por comprension

--cambios' l = [(x, xpos) | (x, xpos) <- zip l [0..], x == xpos]
cambios' l = [xpos | (x, xpos) <- zip l [0..], (y, ypos) <- zip l [0..], ypos == xpos + 1 && x /= y] --Version con listas por comprension

--cambios l = [ pos | (e,pos) <- zip l [0..], ]

--testeo::[(Int,Int)] -> [(Int,Int)]
--testeo l = l
--testeo l = [ e | e <- l]


--prueba'::(Eq a) => a -> a
--prueba' l = l