module World.GameEvent where

import Graphics.Gloss

data GameEvent = JumpmanCollision Vector
               | TimedEvent Float GameEvent
               | Quit
