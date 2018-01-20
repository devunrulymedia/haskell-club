module Block where

import Shape
import Graphics.Gloss
import Renderable

data Block = Block Shape Color deriving (Show, Eq)

instance Renderable Block where
  render (Block shape col) = color col $ render shape
