data Result = Empty | Sum Int | Inconsistent deriving Show

isMagic :: [[Int]] -> Bool
isMagic xs = case foldl magicSoFar Empty (sets xs) of
  (Sum _) -> True
  otherwise -> False

magicSoFar :: Result -> [Int] -> Result
magicSoFar Empty xs = Sum (sum xs)
magicSoFar Inconsistent xs = Inconsistent
magicSoFar (Sum count) xs
  | (sum xs) == count = Sum count
  | otherwise = Inconsistent

sets :: [[Int]] -> [[Int]]
sets square = [rows, columns, diagonals] >>= ($ square)

rows :: [[Int]] -> [[Int]]
rows = id

columns :: [[Int]] -> [[Int]]
columns ([]:rows) = [] 
columns xs = (map head xs) : (columns (map tail xs))

diagonals :: [[Int]] -> [[Int]]
diagonals xs = [diagonal' xs, diagonal' $ reverse xs] where
  diagonal' [] = []
  diagonal' ((x:_):ys) = x : diagonal' (map tail ys)
