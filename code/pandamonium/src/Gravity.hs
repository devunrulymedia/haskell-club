module Gravity where

import Vector
import Ball
import World

gravitate :: Float -> Float -> World -> World
gravitate g t w = w { ball = newBall } where
  oldBall = ball w
  newBall = oldBall { velocity = velocity oldBall + Vector { x = 0, y = -g } * scale t }
