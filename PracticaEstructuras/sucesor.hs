module Suc where

data Nat = Zero | Succ Nat deriving Show
ej1 = Succ (Succ Zero) 
ej2 = Succ (Succ (Succ (Succ Zero)) )
--Representacion de numeros naturales con Nat
{-
0 --> Zero
1 --> Succ Zero (el sucesor de 0 es 1)
2 --> Succ (Succ Zero) (el sucesor del sucesor de 0 es 2)
-}
--revisar este codigo
addSucc :: Nat -> Nat -> Int 
addSucc n Zero = n  
addSucc n (Succ m) = Succ (addSucc n m) 


--Succ (Succ Zero) + Succ Zero = Succ (Succ (Succ Zero))
--       2        +     1     = 3 
{-
addSucc Succ (Succ Zero) Succ Zero = Succ (addSucc n m)
                                   = Succ (Succ add (Succ Zero) Zero)
                                   = Succ (Succ (Succ Zero))
-}


natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

intToNat :: Int -> Nat 
intToNat 0 = Zero 
intToNat n = Succ (intToNat (n - 1))

-- "Recordar que la multiplicacion no es mas que aplicar la suma varias veces"
multNat :: Nat -> Nat -> Nat 
multNat n1 n2 = intToNat (natToInt n1 * natToInt n2) 

-- "Recordar que la exponenciacion no es mas que aplicar la multiplicacion varias veces"
-- expNat :: Nat -> Int -> Nat
-- expNat Zero _ = Zero
-- expNat s1 n = 