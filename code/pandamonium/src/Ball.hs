module Ball where

import Shape
import Renderable
import Graphics.Gloss

data Ball = Ball Vector Vector deriving (Show, Eq)

instance Shaped Ball where
  shape (Ball pos _) = Circ (Shape.Circle pos 10)

instance Renderable Ball where
  render ball = color yellow $ render $ shape ball
