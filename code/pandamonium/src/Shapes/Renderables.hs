module Shapes.Renderables where

import Shapes.Datatypes
import Renderable
import Graphics.Gloss (translate, rectangleSolid, circleSolid, polygon)

instance Renderable Shape where
  render (Circle (x, y) r) = translate x y $ circleSolid r
  render (Polygon points) = polygon points
  render (Rectangle l r t b) = translate (l + width / 2) (b + height / 2)
                               $ rectangleSolid width height where
                                 width = r - l
                                 height = t - b
