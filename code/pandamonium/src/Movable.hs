module Movable where

import Graphics.Gloss (Vector)

class Movable a where
  move :: a -> Vector -> a

class Moving a where
  velocity :: a -> Vector
  applyImpulse :: a -> Vector -> a
