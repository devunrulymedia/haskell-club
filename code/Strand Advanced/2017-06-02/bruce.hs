
data A = A Int deriving (Show, Eq)

lessThan a b = elem (a,b) [(A 1,A 2),(A 3,A 4)]

instance Ord A where
	compare a b | a `lessThan` b = LT
		    | otherwise = GT
