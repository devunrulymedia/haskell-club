module Game.GameEvent where

import Graphics.Gloss

data GameEvent = JumpmanCollision Vector
               | TimedEvent Float GameEvent
               | ChangeSceneryTo Color 
               | Quit
