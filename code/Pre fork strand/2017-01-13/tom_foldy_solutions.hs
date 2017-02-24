length' :: [a] -> Int
length' = foldl add' 0 where
  add' c _ = c + 1

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

take' :: Int -> [a] -> [a] 
take' x ys = snd $ foldl pick (x, []) ys where
   pick (c, acc) a 
     | c < 0 = error "Cannot take a negative number of items from a list"
     | c == 0 = (c, acc)
     | otherwise = (c-1, acc ++ [a])
