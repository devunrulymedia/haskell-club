data Encoding a = Single a
	        | Multiple Int a
		deriving (Show)

encode :: (Eq a) => [a] -> [Encoding a]
encode [] = []
encode (x:xs) = rle xs (1,x) []

rle :: (Eq a) => [a] -> (Int, a) -> [Encoding a] -> [Encoding a]
rle [] currentPair encoded = encoded ++ [encodingOfPair currentPair]
rle (x:xs) currentPair@(count, element) encoded 
	| x == element = rle xs (count+1, element) encoded
	| otherwise = rle xs (1,x) (encoded ++ [encodingOfPair currentPair])

encodingOfPair :: (Int, a) -> Encoding a
encodingOfPair (count,elem)
		| count == 1 = Single elem
		| otherwise = Multiple count elem




