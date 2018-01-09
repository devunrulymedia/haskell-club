module Block where

import Shape
import Graphics.Gloss
import Renderable

data Block = Block { shape :: Shape, col :: Color } deriving (Show, Eq)

instance Renderable Block where
  render a = color (col a) $ render (shape a)
