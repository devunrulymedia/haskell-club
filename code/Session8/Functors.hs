class Functor' f where
	fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
	fmap' g (Just a) = Just (g a)
	fmap' g Nothing  = Nothing

instance Functor' [] where
	fmap' = map

instance Functor' IO where
	fmap' g a = do
					x <- a
			  	  	return (g x)