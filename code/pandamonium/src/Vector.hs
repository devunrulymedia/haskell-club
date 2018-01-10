module Vector where

data Vector = Vector { x :: Float, y :: Float } deriving (Show, Eq)
type Point = Vector
zero_vector = Vector { x = 0, y = 0 }

move_up :: Float -> Vector
move_up y = zero_vector { y = y }

move_down :: Float -> Vector
move_down y = zero_vector { y = -y }

move_left :: Float -> Vector
move_left x = zero_vector { x = -x }

move_right :: Float -> Vector
move_right x = zero_vector { x = x }

magnitude :: Vector -> Float
magnitude = sqrt . sq_mag

sq_mag :: Vector -> Float
sq_mag vec = x vec * x vec + y vec * y vec

instance Num Vector where
  a + b = Vector { x = x a + x b, y = y a + y b }
  a - b = Vector { x = x a - x b, y = y a - y b }
  a * b = Vector { x = x a * x b, y = y a * y b }
  abs a = a * signum a
  signum a = Vector { x = signum $ x a, y = signum $ y a }
  fromInteger x = Vector { x = fromInteger x, y = 0 }

instance Ord Vector where
  compare a b = compare (sq_mag a) (sq_mag b)
