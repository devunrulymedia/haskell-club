module Shapes.Movables where

import Shapes.Datatypes
import Graphics.Gloss (Vector)

class Movable a where
  move :: a -> Vector -> a

class Moving a where
  velocity :: a -> Vector
  applyImpulse :: a -> Vector -> a

instance Movable Shape where
  move (Rectangle l r t b) (x, y ) = Rectangle (l + x) (r + x) (t + y) (b + y)
  move (Circle c r) v = Circle (c + v) r
