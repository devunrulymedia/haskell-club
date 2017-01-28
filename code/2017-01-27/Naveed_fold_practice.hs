foldLength :: [a] -> Integer
foldLength = (\_ n -> 1 + n) 0

foldSum :: [Integer] -> [Integer]
foldSum = (+) 0
