{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

class Doubleable a where
    double :: a -> a
    quadruple :: a -> a
    quadruple = double . double

instance (Num a) => Doubleable a where
    double n = 2 * n

--fails 
instance Doubleable [a] where
    double s = s ++ s


