reverse' :: [a] -> [a]
reverse' xs = foldr f [] xs
    where f :: a -> [a] -> [a]
          f x xs' = xs' ++ [x]
          
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = foldr f True (zip xs (reverse' xs))
    where f :: (Eq a) => (a,a) -> Bool -> Bool
          f (x,y) t = (x == y) && t


data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
