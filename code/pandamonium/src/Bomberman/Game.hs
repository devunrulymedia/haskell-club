module Bomberman.Game where

import Control.Lens
import Graphics.Gloss (white, yellow)
import Graphics.Gloss.Interface.IO.Game (Event)

import Common.Redux
import Common.Components
import Common.Shapes.Shape

import Bomberman.Bomberman

wall :: Entity
wall = entity
   <-+ rectangleV (-200, -200) (400, 200)
   <-+ white
   <-+ Immovable

initialise :: Events ()
initialise = do
  spawn bomberman
  spawn wall

bombermanGame :: IO World
bombermanGame = reduxDo bombermanRedux (newWorld coloredShape) initialise

bombermanRedux :: Redux World
bombermanRedux = compose [ connect (onAll playerRedux) entities, physics, lifecycle ]
