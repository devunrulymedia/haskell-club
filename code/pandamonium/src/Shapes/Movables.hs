module Shapes.Movables where

import Movable
import Shapes.Datatypes


instance Movable Shape where
  move (Rectangle l r t b) (x, y ) = Rectangle (l + x) (r + x) (t + y) (b + y)
  move (Circle c r) v = Circle (c + v) r
