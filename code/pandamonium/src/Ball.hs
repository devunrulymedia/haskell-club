module Ball where

import Shape
import Renderable
import Graphics.Gloss

data Ball = Ball Vector Vector Picture deriving (Show, Eq)

class Moving a where
  velocity :: a -> Vector
  applyImpulse :: a -> Vector -> a

instance Shaped Ball where
  shape (Ball pos _ _) = Circ (Shape.Circle pos 10)

instance Renderable Ball where
  render (Ball (x, y) vel pic) = color yellow $ translate x y pic

instance Movable Ball where
  move (Ball (x, y) vel pic) (dx, dy) = Ball (x + dx, y + dy) vel pic

instance Moving Ball where
  velocity (Ball _ v _) = v
  applyImpulse (Ball pos (x, y) pic) (dx, dy) = Ball pos (x + dx, y + dy) pic
