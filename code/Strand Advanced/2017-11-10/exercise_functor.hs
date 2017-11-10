
data Point a = Point a a deriving (Show)
instance Functor Point where 
    fmap f (Point x y) = Point (f x) (f x)