module EJ2 where 

data Arb = E | H Int | N Arb Arb deriving Show

data Cmd = L | R deriving (Show,Eq)

a1 = N (N (H 3) (H 4)) (H 5)

--a) 
-- N :: Arb -> Arb -> Arb 

--b)
selec :: [Cmd] -> Arb -> Arb 
selec [] a = a 
selec (x:xs) (N l r) | x == L = selec xs l
                     | otherwise = selec xs r

--c)
enum :: Arb -> [[Cmd]]
enum (H _) = [] 
enum (N l r) = (L : enum l) : (R : enum r)



