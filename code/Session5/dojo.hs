s :: String
s = "teststring"


dropEvery :: [a] -> Int -> [a]
-- dropEvery [] _ = []
dropEvery xl n = dropper xl 1
    where dropper (x:xs) k
              | k == n = dropper xs 1
              | otherwise = x:dropper xs (k + 1)
          dropper [] _ = []

split' :: [a] -> Int -> ([a], [a])
split' [] _ = ([], [])
split' lst n = splitter lst [] n
    where splitter second@(y:ys) first k
                    | k == 0 = (first, second)
                    | otherwise = splitter ys (first ++ [y]) (k-1)

slice' :: [a] -> Int -> Int -> [a]
slice' xs i j = first'
                where (_, last) = split' xs i
                      (first',_) = split' last (j-i)

split'' xs n = (take n xs, drop n xs)
