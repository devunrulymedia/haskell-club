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
import Player
import Data.Maybe

data World = World
                { scenery :: [ Block ]
                , players :: [Player]
                , ball :: Ball
                , initialBall :: Ball
                , events :: [ GameEvent ]
                }

instance Renderable World where
  render world = Pictures $
                  (render <$> scenery world) ++
                  [(render $ ball world)] ++
                  (render <$> players world)

gravitate :: Float -> Float -> World -> World
gravitate g t w@World { ball = (Ball pos vel pic) } = w { ball = Ball pos (vel + mulSV t (0, -g)) pic }

integrate :: Float -> World -> World
integrate t w@World { ball = (Ball pos vel pic) } = w { ball = Ball (pos + mulSV t vel) vel pic }

updatePlayers :: Float -> World -> World
updatePlayers t world = world { players = update t <$> players world }

increase :: Score -> Score
increase (Score pos points) = Score pos (points + 1)

checkForScore :: Float -> World -> World
checkForScore t world = let newEvents = players world >>= checkScore'
                         in world { events = newEvents ++ events world } where
  checkScore' :: Player -> [ GameEvent ]
  checkScore' player = if shape (ball world) !!! shape (endzone player)
    then [ PointScored $ index player ]
    else []

resetBall :: Float -> World -> World
resetBall t world = case events world of
  []                     -> world
  (PointScored i : rest) -> world
                              { events = rest
                              , ball = initialBall world
                              , players = scorePoint <$> players world } where
      scorePoint :: Player -> Player
      scorePoint player = if index player == i
        then player { score = increase $ score player }
        else player

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
  players = map (\p -> p { paddle = foldl paddleBlockCollision (paddle p) (scenery w) }) (players w),
  ball = foldl ballCollision (ball w) ((shape <$> scenery w) ++ (shape <$> paddle <$> players w)) }

instance Updatable World where
  listen event world = world { players = listen event <$> players world }
  update t world = foldl (\w f -> f t w) world [
         updatePlayers,
         gravitate 400,
         integrate,
         checkForScore,
         resetBall,
         handleCollisions
         ]
