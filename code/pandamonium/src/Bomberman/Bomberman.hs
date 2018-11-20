{-# LANGUAGE DeriveAnyClass #-}

module Bomberman.Bomberman where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game (yellow, Event)

import Common.Redux
import Common.Shapes.Shape (circle)
import Common.Components
import Bomberman.Controller
import Bomberman.Bomb

data BombCount = BombCount Int deriving Component

bomberman :: EntityId -> Entity
bomberman entId = entity
              <-+ Position (100, 100)
              <-+ Mass 1
              <-+ Elasticity 0
              <-+ circle (0, 0) 50
              <-+ yellow
              <-+ BombCount 3
              <-+ defaultController entId

speed :: OnAxis -> Float
speed Min = -500
speed Neutral = 0
speed Max = 500

move :: a -> Controller -> Velocity
move _ controller = let xSpeed = speed (controller ^. horizontal . onAxis)
                        ySpeed = speed (controller ^. vertical . onAxis)
                     in Velocity (xSpeed, ySpeed)

dropBombs :: BombButtonPressed -> Entity -> IOEvents Entity
dropBombs (BombButtonPressed owner) entity = do
  case dropsBombAt of
    Just (x, y, ent) -> do fireEvent (DropBomb (Owner owner) x y); return ent
    Nothing -> return entity
  where
    dropsBombAt :: Maybe (Float, Float, Entity)
    dropsBombAt = do
      entityId <- extract entity
      (BombCount bombs) <- extract entity
      if entityId == owner && bombs > 0
      then do (Position (x, y)) <- extract entity
              let newEntity = entity <-+ BombCount (bombs - 1)
              return (x, y, newEntity)
      else Nothing

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
