module Bomberman.Bomberman where

import Graphics.Gloss (yellow)

import Common.Redux
import Common.Components
import Common.Shapes.Shape

player :: Entity
player = entity
     <-+ Position (100, 100)
     <-+ circle (0, 0) 50
     <-+ yellow

initialise :: Events ()
initialise = do
  spawn $ player

buildBomberman :: IO World
buildBomberman = reduxDo bombermanRedux (newWorld coloredShape) initialise

bombermanRedux :: Redux World
bombermanRedux = compose [ physics, lifecycle ]
