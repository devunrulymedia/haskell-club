module Ball where

import Shape
import Renderable
import Graphics.Gloss

data Ball = Ball Vector Vector deriving (Show, Eq)

instance Renderable Ball where
  render (Ball pos vel) = color yellow $ render $ Shape.Circle pos 10
