module World where

import Ball
import Block
import Paddle
import Renderable
import Updatable
import GameEvent
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Score
import Shape

data World = World { scenery :: [ Block ], paddles :: [ Paddle ], ball :: Ball, initialBall :: Ball, scores :: [ Score ], events :: [ GameEvent ] }

instance Renderable World where
  render world = Pictures $ (render <$> scenery world) ++ (render <$> paddles world) ++ [(render $ ball world)] ++ (render <$> scores world)

gravitate :: Float -> Float -> World -> World
gravitate g t w@World { ball = (Ball pos vel pic) } = w { ball = Ball pos (vel + mulSV t (0, -g)) pic }

integrate :: Float -> World -> World
integrate t w@World { ball = (Ball pos vel pic) } = w { ball = Ball (pos + mulSV t vel) vel pic }

updatePaddles :: Float -> World -> World
updatePaddles t world = world { paddles = update t <$> paddles world }

checkForScore :: Float -> World -> World
checkForScore t world = if any (shape (ball world) !!!) (shape <$> scenery world)
  then world { events = PointScored 0 : events world }
  else world

increase :: Score -> Score
increase (Score pos points) = Score pos (points + 1)

incrementScore :: Float -> World -> World
incrementScore t world = case events world of
  []                     -> world
  (PointScored _ : rest) -> world { events = rest, scores = increase <$> scores world, ball = initialBall world }

ballCollision :: Ball -> Shape -> Ball
ballCollision ball@(Ball pos vel pic) shp = maybe ball handleCollision (shp !!> shape ball) where
  handleCollision pushout = Ball bounced_pos reflected_vel pic where
    unit_push     = normalizeV pushout
    bounced_pos   = pos + (mulSV 2 pushout)
    normal_proj   = 2 * (vel `dotV` unit_push)
    reflected_vel = vel - mulSV normal_proj unit_push

paddleBlockCollision :: Paddle -> Block -> Paddle
paddleBlockCollision paddle block = maybe paddle (move paddle) (shape block !!> shape paddle)

handleCollisions :: Float -> World -> World
handleCollisions t w = w {
  paddles = map (\p -> foldl paddleBlockCollision p (scenery w)) (paddles w),
  ball = foldl ballCollision (ball w) ((shape <$> scenery w) ++ (shape <$> paddles w)) }

instance Updatable World where
  listen event world = world { paddles = listen event <$> paddles world }
  update t world = foldl (\w f -> f t w) world [
         updatePaddles,
         gravitate 400,
         integrate,
         checkForScore,
         incrementScore,
         handleCollisions
         ]
