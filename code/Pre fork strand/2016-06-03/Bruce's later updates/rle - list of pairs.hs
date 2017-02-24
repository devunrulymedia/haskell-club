
encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) = rle xs (1,x) [] 

rle :: (Eq a) => [a] -> (Int, a) -> [(Int, a)] -> [(Int, a)] 
rle [] currentPair encoded = encoded ++ [currentPair]
rle (x:xs) currentPair@(count, element) encoded 
	| x == element = rle xs (count+1, element) encoded
	| otherwise = rle xs (1,x) (encoded ++ [currentPair])


