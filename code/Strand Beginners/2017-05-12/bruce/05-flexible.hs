{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

class Doubleable a where
    double :: a -> a

instance (Num a) => Doubleable a where
    double n = 2 * n

