module Ball where

import Shape
import Renderable
import Graphics.Gloss

data Ball = Ball Vector Vector Picture deriving (Show, Eq)

instance Shaped Ball where
  shape (Ball pos _ _) = Circ (Shape.Circle pos 16)

instance Renderable Ball where
  render (Ball (x, y) vel pic) = color yellow $ translate x y pic
