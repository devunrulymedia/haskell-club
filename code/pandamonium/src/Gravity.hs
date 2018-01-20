module Gravity where

import Ball
import World
import Graphics.Gloss.Data.Vector

gravitate :: Float -> Float -> World -> World
gravitate g t w = w { ball = newBall } where
  oldBall = ball w
  newBall = oldBall { velocity = velocity oldBall + mulSV t (0, -g) }
