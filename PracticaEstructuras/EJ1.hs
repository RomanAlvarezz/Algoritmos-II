module EJ1 where 

data Color = Rojo | Verde | Azul | RGB Float Float Float deriving Show 

mezclar:: Color -> Color -> Color
mezclar (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB ((r1 + r2) / 2) ((g1 + g2) / 2) ((b1 + b2) / 2) --Y si quiero mezclar Rojo con otro color??
