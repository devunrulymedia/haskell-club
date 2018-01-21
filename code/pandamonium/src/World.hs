module World where

import Ball
import Block
import Paddle
import Renderable
import Updatable
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Shape

data World = World { scenery :: [ Block ], paddles :: [ Paddle ], ball :: Ball }

instance Renderable World where
  render world = Pictures $ (render <$> scenery world) ++ (render <$> paddles world) ++ [(render $ ball world)]

gravitate :: Float -> Float -> World -> World
gravitate g t w@World { ball = (Ball pos vel) } = w { ball = Ball pos (vel + mulSV t (0, -g)) }

integrate :: Float -> World -> World
integrate t w@World { ball = (Ball pos vel) } = w { ball = Ball (pos + mulSV t vel) vel }

updatePaddles :: Float -> World -> World
updatePaddles t world = world { paddles = update t <$> paddles world }

handle :: Ball -> Block -> Ball
handle ball@(Ball pos vel) block = maybe ball handleCollision (shape block !!> shape ball) where
  handleCollision pushout = Ball bounced_pos reflected_vel where
    unit_push     = normalizeV pushout
    bounced_pos   = pos + (mulSV 2 pushout)
    normal_proj   = 2 * (vel `dotV` unit_push)
    reflected_vel = vel - mulSV normal_proj unit_push

handleCollisions :: Float -> World -> World
handleCollisions t w = w { ball = foldl handle (ball w) (scenery w) }

instance Updatable World where
  listen event world = world { paddles = listen event <$> paddles world }
  update t world = foldl (\w f -> f t w) world [updatePaddles, gravitate 400, integrate, handleCollisions]
