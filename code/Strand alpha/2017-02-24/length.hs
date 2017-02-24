

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length xs

fact' :: Int -> Int
fact' 0 = 1
fact' n = n * fact' (n - 1)


