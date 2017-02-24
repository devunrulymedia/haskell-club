foldLength :: [a] -> Integer
foldLength = foldr (\_ n -> 1 + n) 0

foldSum :: [Integer] -> Integer
foldSum = foldr (+) 0
