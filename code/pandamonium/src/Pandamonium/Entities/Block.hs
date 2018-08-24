module Pandamonium.Entities.Block where

import Common.Shapes.Shape
import Graphics.Gloss (Color, color)
import Common.Renderable

data Block = Block Shape Color deriving (Show, Eq)

instance Shaped Block where
  shape (Block s _) = s

instance Renderable Block where
  render (Block s c) = color c $ render s
