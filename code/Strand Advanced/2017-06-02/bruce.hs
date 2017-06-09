
data A = A Int deriving (Show, Eq)

lessThan a b = elem (a,b) [(A 1,A 2),(A 3,A 4)]

instance Ord A where
	compare a b | a `lessThan` b = LT
		    | otherwise = GT

-- now you can do:
-- Prelude Data.List>sort [A 1, A 5, A 4, A 3]
