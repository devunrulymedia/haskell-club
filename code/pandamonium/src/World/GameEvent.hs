module World.GameEvent where

import Graphics.Gloss

data GameEvent = JumpmanCollision Vector
               | Quit
