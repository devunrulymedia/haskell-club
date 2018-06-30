module Shapes.Shape (
  module Shapes.Shapes,
  module Shapes.Collisions,
  module Shapes.Movables,
  module Shapes.Renderables
) where

import Shapes.Shapes (Shape, Shaped (shape), polygon, rectangle, circle)
import Shapes.Collisions
import Shapes.Movables
import Shapes.Renderables
