length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' xs = rev xs [] where
  rev [] acc = acc
  rev (x:xs) acc = rev xs (x:acc)

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) 
  | n < 0 = error "cannot take negative elements from a list"
  | otherwise = x:(take' (n - 1) xs)
