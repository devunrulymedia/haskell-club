import Test.QuickCheck

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
	| length ys > 10 = []
	| x == y = helper (Multiple (n+1) x) ys 
	| otherwise = rle : (helper (Multiple 1 y) ys)

decode :: [Rle a] -> [a]
decode [] = []
decode ((Multiple n a):xs) = (replicate n a) ++ (decode xs)

{-*

Run some property-based tests in the repl:

*Main> quickCheck ((\s -> decode(encode s) == s) :: [Char] -> Bool)
*** Failed! Falsifiable (after 17 tests and 14 shrinks):     
"aaaaaaaaaaaaa"
*Main>

You can use verboseCheck in place of quickCheck.

-}
