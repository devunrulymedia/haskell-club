pack :: (Eq a) => [a] -> [a]

pack [] = []
pack [x] = [x]
pack (x:y:xs) 
    | x==y = x : pack (y:xs)
    | otherwise = pack [x]