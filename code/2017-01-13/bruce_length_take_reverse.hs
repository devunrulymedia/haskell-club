
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr (\_ acc -> acc+1 ) 0

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) 
	| n < 0     = x:xs
	| otherwise = x:(take' (n-1) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]






