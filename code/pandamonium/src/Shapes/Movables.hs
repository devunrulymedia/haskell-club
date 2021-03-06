module Shapes.Movables where

import Shapes.Datatypes
import Graphics.Gloss.Data.Vector

class Movable a where
  move :: Vector -> a -> a

class Movable a => Moving a where
  velocity :: a -> Vector
  applyImpulse :: Vector -> a -> a
  applyVelocity :: Float -> a -> a
  applyVelocity t a = move (mulSV t (velocity a)) a

instance Movable Shape where
  move (x, y) (Rectangle l r t b) = Rectangle (l + x) (r + x) (t + y) (b + y)
  move v (Circle c r) = Circle (c + v) r
