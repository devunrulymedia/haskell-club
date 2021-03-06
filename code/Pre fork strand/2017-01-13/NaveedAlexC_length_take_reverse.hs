length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

take' :: [a] -> Int -> [a]
take' [] _ = []
take' _ 0 = []
take' (x:xs) y 
      | y < 0     = x:xs
      | otherwise = x:(take' xs (y-1))

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]
