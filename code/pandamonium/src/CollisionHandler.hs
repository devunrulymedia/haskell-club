module CollisionHandler where

import Data.Maybe
import Ball
import Block
import Shape
import World
import Graphics.Gloss.Data.Vector

handle :: Ball -> Block -> Ball
handle ball@(Ball pos vel) block = maybe ball handleCollision (shape block !!> shape ball) where
  handleCollision pushout = Ball bounced_pos reflected_vel where
    unit_push     = normalizeV pushout
    bounced_pos   = pos + (mulSV 2 pushout)
    normal_proj   = 2 * (vel `dotV` unit_push)
    reflected_vel = vel - mulSV normal_proj unit_push

handleCollisions :: Float -> World -> World
handleCollisions t w = w { ball = foldl handle (ball w) (scenery w) }
