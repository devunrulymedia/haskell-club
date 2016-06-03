data Encoding a = Single a
	        | Multiple Int a
		deriving (Show)

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) = rle xs (1,x) [] 

rle :: (Eq a) => [a] -> (Int, a) -> [(Int, a)] -> [(Int, a)] 
rle [] currentPair encoded = encoded ++ [currentPair]
rle (x:xs) currentPair@(count, element) encoded 
	| x == element = rle xs (count+1, element) encoded
	| otherwise = rle xs (1,x) (encoded ++ [currentPair])


encode' :: (Eq a) => [a] -> [Encoding a]
encode' [] = []
encode' (x:xs) = rle' xs (1,x) []

rle' :: (Eq a) => [a] -> (Int, a) -> [Encoding a] -> [Encoding a]
rle' [] currentPair encoded = encoded ++ [encodingOfCurrentPair currentPair]
	where encodingOfCurrentPair (count,elem)
		|  count == 1 = Single elem
		| otherwise = Multiple count elem


