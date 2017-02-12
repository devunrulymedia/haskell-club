hello :: String
hello = "Hello World"

remove_dups :: Eq a => [a] -> [a]
remove_dups [] = []
remove_dups (x:xs)
    | xs == [] = [x]
    | x == head xs = remove_dups xs
    | otherwise = x:(remove_dups xs)

remove_dups' :: Eq a => [a] -> [a]
remove_dups' [] = []
remove_dups' (x:xs) = x:remove_dups'(dropWhile (== x) xs)



group_dups :: Eq a => [a] -> [[a]]
group_dups [] = []
group_dups (x:xs) = (x:takeWhile (== x) xs) : group_dups (dropWhile (== x) xs)

rle :: Eq a => [a] -> [(a,Int)]
rle list = map length' (group_dups list)
 
length' :: [a] -> (a, Int)
length' all@(x:xs) = (x, length all)
