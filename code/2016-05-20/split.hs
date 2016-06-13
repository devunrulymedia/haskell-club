-- split takes a list and two indices
-- returns a list of elements between the two indices
split :: (Ord a, Num a, Enum a) => [t] -> a -> a -> [t]
split [] _ _ = []
split xs s e
     | s > e = []
     | otherwise = [ b | (a,b) <- (zip [1..] xs), a >= s, a <= e]
