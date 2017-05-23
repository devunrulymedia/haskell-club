{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

class Doubleable a where
    double :: a -> a

instance Doubleable a where
    double n = n

--fails 
instance Doubleable [a] where
    double s = s ++ s


