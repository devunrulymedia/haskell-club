module Shapes.Renderables where

import Shapes.Shapes
import Renderable
import Graphics.Gloss (translate, rectangleSolid, circleSolid, polygon)

instance Renderable Shape where
  render (Circle (x, y) r) = translate x y $ circleSolid r
  render (Polygon points _) = Graphics.Gloss.polygon points
