{-# LANGUAGE MultiParamTypeClasses #-}

module Pandamonium.Game.GameEvent where

import Graphics.Gloss.Data.Vector

data GameEvent = ResetCollisions
               | PointsScored Int
               | JumpPressed
               | RespawnCoin Integer Vector
               | ChangeScenery
               | Cleared
