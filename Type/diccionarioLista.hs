newtype Dictionary a = Dict [a] deriving Show

mkNewDict :: Dictionary a 
mkNewDict = Dict []

insertDict :: (Ord a) => a -> Dictionary a -> Dictionary a 
insertDict el (Dict []) = Dict [el] 
insertDict el (Dict d) = Dict (el:d) 

inDict :: (Ord a) => a -> Dictionary a -> Bool 
inDict _ (Dict []) = False 
inDict el (Dict d) = elem el d

delDict :: (Ord a) => a -> Dictionary a -> Dictionary a
delDict _ (Dict []) = Dict []
delDict el (Dict d) = Dict [x | x <- d, x /= el] 

