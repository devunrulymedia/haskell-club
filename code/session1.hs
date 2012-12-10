first :: [a] -> a
first (x:xs) = x

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

incby1 :: Num a => [a] -> [a]
incby1 [] = []
incby1 (x:xs) = (x + 1):(incby1 xs) 

map' :: [a] -> (a->b) -> [b]
map' [] _ = []
map' (x:xs) f = f(x):(map' xs f)
