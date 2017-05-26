magicsquare = [[2,9,4],[7,5,3],[6,1,8]] :: [[Int]]
square = [[1,2,3],[4,5,6],[7,8,9]] :: [[Int]]

isMagic :: [[Int]] -> Bool
isMagic square = allEqual $ map sum $ sets square

allEqual :: (Eq a) => [a] -> Bool
allEqual (x:y:rest) = x == y && allEqual (y:rest)
allEqual _ = True

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
