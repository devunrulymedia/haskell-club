module Block where

import Shape
import Graphics.Gloss (Color, color)
import Renderable
import Shapes.Renderables

data Block = Block Shape Color deriving (Show, Eq)

instance Shaped Block where
  shape (Block s _) = s

instance Renderable Block where
  render (Block s c) = color c $ render s