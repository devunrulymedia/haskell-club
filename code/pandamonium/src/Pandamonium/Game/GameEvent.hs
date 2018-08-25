module Pandamonium.Game.GameEvent where

import Graphics.Gloss
import Pandamonium.Game.Timer

data GameEvent = ResetCollisions
               | PandaCollision Vector
               | JumpPressed
               | Trigger Float GameEvent
               | CoinPickedUp String
               | RespawnCoin String Vector
               | ChangeScenery
               | Cleared
               | Quit

instance TimedEvent GameEvent where
  timed (Trigger t a) = Just $ DueIn t a
  timed _ = Nothing
