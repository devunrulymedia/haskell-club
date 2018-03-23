
ziplike :: [a] -> [b] -> [(a, b)]
ziplike []     _      = []
ziplike _      []     = []
ziplike (a:as) (b:bs) = (a, b) : ziplike as bs

-- splitAtIndex 3 [1,1,1,2,2,2]
splitAtIndex :: Int -> [a] -> ([a],[a])
splitAtIndex k xs = (take k xs, drop k xs)

-- dropK 3 [1,2,3,4,5,6] = [1,2,3,5,6]
dropK :: Int -> [a] -> [a]
dropK k xs = take k xs ++ drop (k+1) xs

-- slice 2 4 [1,2,3,4,5,6] = [3,4]
slice :: Int -> Int -> [a] -> [a]
slice i k xs = drop i (take k xs)

foo a b = a + b

-- insertElem 9 2 [1,2,3,4,5] = [1,2,9,3,4,5]
insertElem x i xs = take i xs ++ x : drop i xs

data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet deriving (Eq, Ord, Bounded, Enum, Show)

-- firstColor = Red
firstColor = minBound :: Color

getFirstColor :: Char	
getFirstColor = maxBound

-- reverseColorOrder = [Violet, Indigo, Blue, Green, Yellow, Orange, Red]
reverseColorOrder = reverse $ enumFrom Red


