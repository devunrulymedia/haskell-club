import Data.List

testdata = [[1,2,3],[4,5,6],[7,8,9]]
magicdata = [[4,9,2],[3,5,7],[8,1,6]]

isEmpty :: [[a]] -> Bool
isEmpty xs = sum (map length xs) == 0

columns :: [[Integer]] -> [[Integer]]
columns xs
  | isEmpty xs = []
  | otherwise  = (map head xs) : columns (map tail xs) 

diagonal :: [[Integer]] -> [Integer]
diagonal xs 
  | isEmpty xs = []
  | otherwise  = (head (head xs)) : diagonal (tail (map tail xs))  

diagonals :: [[Integer]] -> [[Integer]]
diagonals xs = [ (diagonal xs), diagonal (map reverse xs) ]

alllines :: [[Integer]] -> [[Integer]]
alllines xs = xs ++ (columns xs) ++ (diagonals xs)

ismagic xs = length (nub (map sum (alllines xs))) == 1 
