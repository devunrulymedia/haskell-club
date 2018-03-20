module Shapes.Renderables where

import Shapes.Shape
import Renderable
import Graphics.Gloss (translate, rectangleSolid, circleSolid)

instance Renderable Rectangle where
  render (Rectangle l r t b) = translate (l + width / 2) (b + height / 2)
                             $ rectangleSolid width height where
                             width = r - l
                             height = t - b

instance Renderable Circle where
  render (Circle (x, y) r) = translate x y $ circleSolid r

instance Renderable Shape where
  render (Rect r) = render r
  render (Circ c) = render c
