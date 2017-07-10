import Test.QuickCheck
import Test.QuickCheck.Property

data Rle a = Multiple Int a

instance (Show a) => Show (Rle a) where
  show (Multiple n a)
	| n == 1 = "(" ++ (show a) ++ ")"
	| otherwise = "(" ++ (show n) ++ " " ++ (show a) ++ ")"

encode ::  [Char] -> [Rle Char]
encode [] = []
encode (x:xs) = helper (Multiple 1 x) xs  

helper ::  Rle Char -> [Char] -> [Rle Char]
helper x [] = [x]
helper rle@(Multiple n x) (y:ys)
	| (ys == "cheese") = []
	| x == y = helper (Multiple (n+1) x) ys 
	| otherwise = rle : (helper (Multiple 1 y) ys)

decode :: [Rle a] -> [a]
decode [] = []
decode ((Multiple n a):xs) = (replicate n a) ++ (decode xs)

{-*

Property tests (probably) won't find the cheese:

*Main> quickCheck ((\s -> decode(encode s) == s) :: [Char] -> Bool)
+++ OK, passed 100 tests.
*Main>

-}
