{-# LANGUAGE DeriveAnyClass #-}

module Bomberman.Bomberman where

import Control.Monad.Trans
import Control.Lens
import Graphics.Gloss.Interface.IO.Game (Color, yellow, red, Event)

import Common.Monad
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

regainBomb :: Exploded -> Entity -> IOEvents Entity
regainBomb (Exploded (Owner ownerId) _ _) entity = if extract entity /= Just ownerId
  then return entity
  else return $ update addBomb entity where
    addBomb :: BombCount -> BombCount
    addBomb (BombCount x) = BombCount (x + 1)

dropBombs :: BombButtonPressed -> Entity -> IOEvents Entity
dropBombs (BombButtonPressed owner) entity = do
  case dropsBombAt of
    Just (x, y, ent) -> do
      fireEvent (DropBomb (Owner owner) x y)
      return ent
    Nothing -> return entity
  where
    dropsBombAt :: Maybe (Float, Float, Entity)
    dropsBombAt = do
      entityId <- extract entity
      (BombCount bombs) <- extract entity
      ifMaybe (entityId == owner && bombs > 0) $ do
        (Position (x, y)) <- extract entity
        let newEntity = entity <-+ BombCount (bombs - 1)
        return (x, y, newEntity)

playerRedux :: Redux Entity
playerRedux = Redux
  { updater = purely2 (update1 move)
  , listener = updateM1 listenController
  , reducer = composeHandler [ focusM dropBombs, focusM regainBomb ]
  }
