module Bomberman.Bomberman where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game (yellow, Event)

import Common.Redux
import Common.Shapes.Shape (circle)
import Common.Components
import Bomberman.Controller

bomberman :: Entity
bomberman = entity
        <-+ Position (100, 100)
        <-+ Mass 1
        <-+ Elasticity 0
        <-+ circle (0, 0) 50
        <-+ yellow
        <-+ defaultController (EntityId 0)

speed :: OnAxis -> Float
speed Min = -500
speed Neutral = 0
speed Max = 500

move :: a -> Controller -> Velocity
move _ controller = let xSpeed = speed (controller ^. horizontal . onAxis)
                        ySpeed = speed (controller ^. vertical . onAxis)
                     in Velocity (xSpeed, ySpeed)

updateBomberman :: Float -> Entity -> Events Entity
updateBomberman time entity = return entity
                          <&> update1 move time

listenBomberman :: Event -> Entity -> Events Entity
listenBomberman event entity = return entity
                           >>= updateM1 listenController event

playerRedux :: Redux Entity
playerRedux = Redux
  { updater = updateBomberman
  , listener = listenBomberman
  , reducer = noOp
  }
