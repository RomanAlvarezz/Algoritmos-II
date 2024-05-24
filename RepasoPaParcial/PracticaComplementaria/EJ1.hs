module EJ1 where

data Nat = Zero | Succ Nat

--a) Succ :: Nat -> Nat

--b)
int2Nat 0 = Zero 
int2Nat n = Succ (int2Nat (n-1))

--c) 
suma Zero n2 = n2 
suma (Succ n1) n2 = Succ (suma n1 n2) 

--d)
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n 