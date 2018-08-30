{-# LANGUAGE MultiParamTypeClasses #-}

module Pandamonium.Game.GameEvent where

import Graphics.Gloss
import Common.Timer
import Common.Entities.Destroyer
import Common.Physics.Collisions
import Pandamonium.Entities.EntityTypes

data GameEvent = ResetCollisions
               | PointsScored Int
               | JumpPressed
               | RespawnCoin Integer Vector
               | ChangeScenery
               | Cleared
