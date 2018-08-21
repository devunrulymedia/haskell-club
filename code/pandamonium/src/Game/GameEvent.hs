module Game.GameEvent where

import Graphics.Gloss

data GameEvent = ResetCollisions
               | PandaCollision Vector
               | JumpPressed
               | TimedEvent Float GameEvent
               | CoinPickedUp String
               | RespawnCoin String Vector
               | ChangeScenery
               | Cleared
               | Quit
