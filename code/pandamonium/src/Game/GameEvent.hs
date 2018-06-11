module Game.GameEvent where

import Graphics.Gloss

data GameEvent = ResetCollisions
               | JumpmanCollision Vector
               | TimedEvent Float GameEvent
               | CoinPickedUp String
               | RespawnCoin String Vector
               | ChangeScenery
               | Quit
