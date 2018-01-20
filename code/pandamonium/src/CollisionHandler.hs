module CollisionHandler where

import Data.Maybe
import Ball
import Block
import Shape
import World
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

handle :: Ball -> Block -> Ball
handle ball (Block shape _) = maybe ball handleCollision (shape !!> Circ (Shape.Circle (pos ball) 10)) where
  handleCollision pushout = Ball { pos = bounced_pos, velocity = reflected_vel } where
    unit_push     = normalizeV pushout
    bounced_pos   = pos ball + (mulSV 2 pushout)
    normal_proj   = 2 * ((velocity ball) `dotV` unit_push)
    reflected_vel = velocity ball - mulSV normal_proj unit_push

handleCollisions :: Float -> World -> World
handleCollisions t w = w { ball = foldl handle (ball w) (scenery w) }
