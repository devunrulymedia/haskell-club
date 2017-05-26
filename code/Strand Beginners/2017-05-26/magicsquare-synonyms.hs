type Row = [Int]
type Square = [Row]
type Line = [Int]

isMagic :: Square -> Bool
isMagic square = allEqual $ map sum $ linesIn square

allEqual :: (Eq a) => [a] -> Bool
allEqual (x:y:xs) = x == y && allEqual (y:xs)
allEqual _ = True

linesIn :: Square -> [Line]
linesIn square = [rows, columns, diagonals] >>= ($ square)

rows :: Square -> [Line]
rows = id

columns :: Square -> [Line]
columns ([]:rows) = [] 
columns xs = (map head xs) : (columns (map tail xs))

diagonals :: Square -> [Line]
diagonals xs = [diagonal' xs, diagonal' $ reverse xs] where
  diagonal' [] = []
  diagonal' ((x:xs):ys) = x : diagonal' (map tail ys)
