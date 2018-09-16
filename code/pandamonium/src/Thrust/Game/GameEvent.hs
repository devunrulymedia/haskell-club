{-# LANGUAGE DeriveAnyClass #-}

module Thrust.Game.GameEvent where

import Graphics.Gloss
import Common.Redux

data GameEvent = ThrusterCollision Vector deriving (Show, ReduxEvent)
