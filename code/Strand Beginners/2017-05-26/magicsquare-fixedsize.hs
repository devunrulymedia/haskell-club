data Line = Line Int Int Int deriving Show
data Row = Row Int Int Int deriving Show
data Square = Square Row Row Row deriving Show

magicSquare :: Square -> Bool
magicSquare s = allEqual (map sumline $ getlines s)

allEqual :: (Eq a) => [a] -> Bool
allEqual (x:y:rest) = x == y && (allEqual (y:rest))
allEqual _ = True

-- the imperative way!
getlines :: Square -> [Line]
getlines square = (rows square) ++ (columns square) ++ (diagonals square) 

-- the first attempt at mapping
getlines1 :: Square -> [Line]
getlines1 square = concat $ map ($ square) [rows, columns, diagonals]

-- applicatives
getlines2 :: Square -> [Line]
getlines2 square = concat $ [rows, columns, diagonals] <*> [square]

-- flatmap that shit
getlines' :: Square -> [Line]
getlines' square = [rows, columns, diagonals] >>= ($ square)

sumline :: Line -> Int
sumline (Line x y z) = x + y + z

rows :: Square -> [Line]
rows (Square a b c) = map asLine [a,b,c] where
  asLine (Row a b c) = Line a b c

columns :: Square -> [Line]
columns (Square (Row a b c) (Row d e f) (Row g h i)) = [Line a d g, Line b e h, Line c f i]

diagonals :: Square -> [Line]
diagonals (Square (Row a b c) (Row d e f) (Row g h i)) = [Line a e i, Line c e g] 
