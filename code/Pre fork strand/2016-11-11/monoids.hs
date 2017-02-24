import Data.Monoid

allTrue :: [Bool] -> Bool
allTrue xs = getAll $ mconcat $ map All xs

someTrue :: [Bool] -> Bool
someTrue xs = getAny $ mconcat $ map Any xs
