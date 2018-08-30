{-# LANGUAGE MultiParamTypeClasses #-}

module Pandamonium.Game.GameEvent where

import Graphics.Gloss
import Common.Timer
import Common.Entities.Destroyer
import Common.Physics.Collisions
import Pandamonium.Entities.EntityTypes

data GameEvent = ResetCollisions
               | Collision EntityType Integer EntityType Integer Vector
               | PointsScored Int
               | JumpPressed
               | Trigger Float GameEvent
               | RespawnCoin Integer Vector
               | Destroy Integer
               | ChangeScenery
               | Cleared

instance TimedEvent GameEvent where
  timed (Trigger t a) = Just $ DueIn t a
  timed _ = Nothing

instance CollisionEvent EntityType Integer GameEvent where
  collisionEvent t1 i1 t2 e2 v = Collision t1 i1 t2 e2 v

instance Destroys Integer GameEvent where
  destroys (Destroy x) = Just x
  destroys _ = Nothing
