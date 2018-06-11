module Game.GameEvent where

import Graphics.Gloss

data GameEvent = ResetCollisions
               | JumpmanCollision Vector
               | JumpPressed
               | TimedEvent Float GameEvent
               | CoinPickedUp String
               | RespawnCoin String Vector
               | ChangeScenery
               | Quit
