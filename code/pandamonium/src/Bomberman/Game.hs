module Bomberman.Game where

import Control.Lens
import Graphics.Gloss (white, yellow)
import Graphics.Gloss.Interface.IO.Game (Event)

import Common.Redux
import Common.Components
import Common.Shapes.Shape

import Bomberman.Bomberman
import Bomberman.Grid

initialise :: Events ()
initialise = do
  spawnWithId bomberman
  createGrid

bombermanGame :: IO World
bombermanGame = reduxDo bombermanRedux (newWorld coloredShape) initialise

bombermanRedux :: Redux World
bombermanRedux = compose [ connect (onAll playerRedux) entities, physics, lifecycle ]
