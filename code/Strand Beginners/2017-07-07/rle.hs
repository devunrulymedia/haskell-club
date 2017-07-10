data Rle a = Multiple Int a

instance (Show a) => Show (Rle a) where
  show (Multiple n a)
	| n == 1 = "(" ++ (show a) ++ ")"
	| otherwise = "(" ++ (show n) ++ " " ++ (show a) ++ ")"

encode :: (Eq a) => [a] -> [Rle a]
encode [] = []
encode (x:xs) = helper (Multiple 1 x) xs  

helper :: (Eq a) => Rle a -> [a] -> [Rle a]
helper x [] = [x]
helper rle@(Multiple n x) (y:ys)
	| x == y = helper (Multiple (n+1) x) ys 
	| otherwise = rle : (helper (Multiple 1 y) ys)

decode :: [Rle a] -> [a]
decode [] = []
decode ((Multiple n a):xs) = (replicate n a) ++ (decode xs)
