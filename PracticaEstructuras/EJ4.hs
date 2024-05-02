module EJ4 where 

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

-- 2 * 3 / 3 = 2
ej1 = Div (Prod (Num 2) (Num 3)) (Num 3) 
ej2 = Prod (Div (Num 5) (Num 2)) (Prod (Num 33) (Num 2))

eval:: Aexp -> Int
eval (Num x) = x 
eval (Prod e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2 

--seval:: Aexp -> Maybe Int  
seval (Num 0) = Nothing
seval (Num n) = Just n 
seval (Prod n1 n2) = case (seval n1, seval n2) of
                        (_, Nothing) -> Nothing
                        (Nothing,_) -> Nothing 
                        (Just x, Just y) -> Just (x*y)

seval (Div n1 n2) = case (seval n1, seval n2) of 
                        (Nothing, _) -> Nothing
                        (_,Nothing) -> Nothing
                        (Just x, Just y) -> Just (x `div` y) 

