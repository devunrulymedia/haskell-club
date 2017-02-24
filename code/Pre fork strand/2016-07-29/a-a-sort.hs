import Data.List (sort)
 
lsort a = map (\(n, l) -> l) $ sort $ map (\l -> (length l, l)) a