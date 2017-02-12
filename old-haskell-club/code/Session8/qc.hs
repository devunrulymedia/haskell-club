import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs

class Functor' f where
	fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
	fmap' g (Just a) = Just (g a)
	fmap' g Nothing  = Nothing
{-
instance functor' [] where
	fmap' = map
-}
prop_identity_maybe x = fmap' $ id (Just x) == fmap' $ id x 
