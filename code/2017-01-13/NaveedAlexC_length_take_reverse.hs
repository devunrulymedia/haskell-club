length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

take' :: [a] -> Int -> [a]
take' [] _ = []
take' _ 0 = []
take' (x:xs) y = x:(take' xs (y-1))