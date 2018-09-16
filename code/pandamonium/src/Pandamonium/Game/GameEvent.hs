{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

module Pandamonium.Game.GameEvent where

import Graphics.Gloss.Data.Vector
import Common.Redux

data GameEvent = ResetCollisions
               | PointsScored Int
               | JumpPressed
               | RespawnCoin Integer Vector
               | ChangeScenery
               | Cleared
               deriving (Show, ReduxEvent)
