module Gravity where

import Ball
import World
import Graphics.Gloss.Data.Vector

gravitate :: Float -> Float -> World -> World
gravitate g t w@World { ball = (Ball pos vel) } = w { ball = Ball pos (vel + mulSV t (0, -g)) }
