addTuplePoint :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuplePoint (a, b) (c, d) = (a + c, b + d)

data Point = Point Int Int deriving (Show, Eq)

addPoint :: Point -> Point -> Point
addPoint (Point a b) (Point c d) = Point (a+c) (b+d)
