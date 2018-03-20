module Shapes.Movables where

import Movable
import Shapes.Datatypes

instance Movable Shape where
  move (Rect l r t b) (x, y ) = Rect (l + x) (r + x) (t + y) (b + y)
  move (Circ c r) v = Circ (c + v) r
