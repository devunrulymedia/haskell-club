
class Doubleable a where
    double :: a -> a
    quadruple :: a -> a
    quadruple = double . double

instance Doubleable Integer where
    double n = 2 * n

instance Doubleable [a] where
    double s = s ++ s

data Point = Point Int Int

instance Doubleable Point where
    double (Point a b) = Point (2*a) (2*b)
    quadruple = double . double . double

instance Show Point where
    show (Point a b) = "Point " ++ show a ++ " " ++ show b

showDouble :: (Doubleable a, Show a) => a -> String
showDouble x = show (double x)