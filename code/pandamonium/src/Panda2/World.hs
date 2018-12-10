module Panda2.World (newGame, panda2Redux) where

import Common.Redux
import Common.Shapes.Shape
import Common.Components

import Panda2.Entities.Block

panda2Redux :: Redux World
panda2Redux = compose [ lifecycle ]

initialiseWorld :: Events ()
initialiseWorld = do
  spawn $ block $ rectangleV (-100, -100) (100, 100)
  return ()

emptyWorld :: World
emptyWorld = newWorld spritesAndShapes

newGame :: IO World
newGame = reduxDo panda2Redux emptyWorld initialiseWorld
