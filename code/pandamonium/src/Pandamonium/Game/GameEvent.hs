{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

module Pandamonium.Game.GameEvent where

import Graphics.Gloss.Data.Vector
import Common.Redux

data GameEvent = PointsScored Int
               | JumpPressed
               | Cleared
               deriving (Show, ReduxEvent)
