type Row = [Int]
type Square = [Row]
type Line = [Int]

data Result = Empty | Sum Int | Inconsistent deriving Show

isMagic :: Square -> Bool
isMagic xs = case foldl magicSoFar Empty (lines' xs) of
  (Sum _) -> True
  otherwise -> False

magicSoFar :: Result -> Line -> Result
magicSoFar Empty xs = Sum (sum xs)
magicSoFar Inconsistent xs = Inconsistent
magicSoFar (Sum count) xs
  | (sum xs) == count = Sum count
  | otherwise = Inconsistent

lines' :: Square -> [Line]
lines' square = concat $ map ($ square) [rows, columns, diagonals]

rows :: Square -> [Line]
rows = id

columns :: Square -> [Line]
columns ([]:rows) = [] 
columns xs = (map head xs) : (columns (map tail xs))

diagonals :: Square -> [Line]
diagonals xs = [diagonal' xs, diagonal' $ reverse xs] where
  diagonal' [] = []
  diagonal' ((x:xs):ys) = x : diagonal' (map tail ys)
