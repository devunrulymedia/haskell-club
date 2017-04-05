
-- write a function to take 3 numerical arguments, and return the minimum of them

min3 ::Int -> (Int -> (Int -> Int))
min3 a b c = min (min a b) c

min3' ::Int -> (Int -> (Int -> Int))
min3' a b = min (min a b)


