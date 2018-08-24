module Pandamonium.Entities.Coin where

import Graphics.Gloss (Color, Vector, color, yellow)
import Common.Shapes.Shape
import Common.Renderable

data Coin = Coin String Vector

instance Shaped Coin where
  shape (Coin _ centre) = circle centre 8

instance Renderable Coin where
  render coin = color yellow $ render $ shape coin
