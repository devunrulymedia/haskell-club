
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x n = (take (n-1) x) 
	++ (dropEvery (takeLastN x ((length x)-n)) n)

takeLastN :: [a] -> Int -> [a]
takeLastN x n = reverse (take (n) (reverse x))

dropEveryThird :: [a] -> [a]
dropEveryThird [] = []
dropEveryThird (x:y:z:xs) = [x,y] ++ dropEveryThird xs
