module Queens (queens) where

import Data.List
import Data.Bool

diagonally_safe' :: [Int] -> [Int] -> [Int] -> Bool
diagonally_safe' [] _ _ = True
diagonally_safe' (q:qs) ls rs = (not $ elem q ls) && (not $ elem q rs) && (diagonally_safe' qs new_ls new_rs) 
                            where
                              new_rs = map (+1) (q:rs) 
                              new_ls = map (\x -> x - 1) (q:ls) 

diagonally_safe :: [Int] -> Bool
diagonally_safe qs = diagonally_safe' qs [] []

-- We generate permutations of queens on different rows, so we know we have no
-- endangerment on rows or columns: all we care about is that the queens are 
-- diagonally safe
queens :: Int -> [[Int]]
queens n = filter diagonally_safe $ permutations [1..n]
