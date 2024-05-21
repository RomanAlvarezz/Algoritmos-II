module EJ1 where 

data Nat = Cero | Succ Nat deriving Show

--a)
--Succ :: Nat -> Nat

--b) 
int2nat :: Int -> Nat 
int2nat 0 = Cero 
int2nat n = Succ (int2nat (n - 1))

--c)
-- 1 + 2 = 3
-- Succ Cero + Succ (Succ Cero) = Succ (Succ (Succ Cero))
suma :: Nat -> Nat -> Nat 
suma Cero nat2 = nat2
suma (Succ n1) n2 = Succ (suma n1 n2)

--d)
-- 3 = Succ (Succ (Succ Cero))
nat2int :: Nat -> Int
nat2int Cero = 0
nat2int (Succ n) = 1 + nat2int n
