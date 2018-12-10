module Panda2.World (newGame, panda2Redux) where

import Common.Redux
import Common.Shapes.Shape
import Common.Components

import Panda2.Assets
import Panda2.Entities.Block
import Panda2.Entities.Panda

panda2Redux :: Redux World
panda2Redux = compose [ lifecycle ]

initialiseWorld :: Assets -> Events ()
initialiseWorld assets = do
  spawn $ block $ rectangleV (-100, -100) (100, 100)
  spawn $ panda assets
  return ()

emptyWorld :: World
emptyWorld = newWorld spritesAndShapes

newGame :: Assets -> IO World
newGame assets = reduxDo panda2Redux emptyWorld (initialiseWorld assets)
