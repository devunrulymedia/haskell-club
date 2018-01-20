module Integrator where
  
import Graphics.Gloss.Data.Vector
import Ball
import World

integrate :: Float -> World -> World
integrate t w = w { ball = newBall } where
  oldBall = ball w
  newBall = oldBall { pos = pos oldBall + mulSV t (velocity oldBall) }
