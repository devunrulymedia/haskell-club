module Panda2.Stage where

import Graphics.Gloss (Picture)
import Common.Redux
import Common.Components
import Common.Shapes.Shape
import Panda2.Entities.Block
import Panda2.Entities.Panda

createStage1 :: [ Picture ] -> Events ()
createStage1 assets = do
  spawn $ block $ rectangleV (-100, -100) (100, 100)
  spawn $ panda assets
  return ()
