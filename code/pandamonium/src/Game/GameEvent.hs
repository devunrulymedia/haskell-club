module Game.GameEvent where

import Graphics.Gloss

data GameEvent = ThrusterCollision Vector
               | TimedEvent Float GameEvent
               | Quit
