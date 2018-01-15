module CollisionHandler where

import Data.Maybe
import Ball
import Block
import Shape
import Vector
import World

handle :: Ball -> Block -> Ball
handle ball block = maybe ball handleCollision ((shape block) !!> Circ (Shape.Circle (pos ball) 10)) where
  handleCollision pushout = Ball { pos = bounced_pos, velocity = reflected_vel } where
    unit_push     = unit pushout
    bounced_pos   = pos ball + (pushout * Vector { x = 2, y = 2 })
    normal_proj   = 2 * ((velocity ball) `dot` unit_push)
    reflected_vel = velocity ball - unit_push * Vector { x = normal_proj, y = normal_proj }

handleWorld :: World -> World
handleWorld w = w { ball = foldl handle (ball w) (scenery w) }
