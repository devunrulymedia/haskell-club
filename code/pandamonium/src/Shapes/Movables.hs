module Shapes.Movables where

import Movable
import Shapes.Datatypes

instance Movable Rectangle where
  move (Rectangle l r t b) (x, y) = Rectangle (l + x) (r + x) (t + y) (b + y)

instance Movable Circle where
  move (Circle c r) v = Circle (c + v) r

instance Movable Shape where
  move (Rect r) v = Rect (move r v)
  move (Circ c) v = Circ (move c v)
