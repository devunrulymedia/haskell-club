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
import Shape
import Collisions
import Player
import Movable
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
                  (render <$> players world) ++
                  [(render $ ball world)]

gravitate :: Float -> Float -> World -> World
gravitate g t w@World { ball = (Ball pos vel pic) } = w { ball = Ball pos (vel + mulSV t (0, -g)) pic }

integrate :: Float -> World -> World
integrate t w@World { ball = (Ball pos vel pic) } = w { ball = Ball (pos + mulSV t vel) vel pic }

updatePlayers :: Float -> World -> World
updatePlayers t world = world { players = update t <$> players world }

checkForScore :: Float -> World -> World
checkForScore t world = let newEvents = players world >>= checkScore'
                         in world { events = newEvents ++ events world } where
  checkScore' :: Player -> [ GameEvent ]
  checkScore' player = if shape (ball world) !!! shape (endzone player)
    then [ PointScored $ index player ]
    else []

resetBall :: GameEvent -> World -> World
resetBall (PointScored _) world = world { ball = initialBall world }

handleEvents :: Float -> World -> World
handleEvents t w = (foldl (flip handleEvent) w (events w)) { events = [] }

instance GameEvents World where
  handleEvent e world = resetBall e world { players = handleEvent e <$> players world }

doCollision :: (Movable a, Moving a, Shaped a) => a -> Shape -> a
doCollision a wall = maybe a handleCollision (wall !!> shape a) where
  handleCollision pushout = move (applyImpulse a reflected_vel) offset where
    vel           = velocity a
    unit_push     = normalizeV pushout
    offset        = mulSV 2 pushout
    normal_proj   = 2 * (vel `dotV` unit_push)
    reflected_vel = negate $ mulSV normal_proj unit_push

paddleBlockCollision :: Paddle -> Block -> Paddle
paddleBlockCollision paddle block = maybe paddle (move paddle) (shape block !!> shape paddle)

handleCollisions :: Float -> World -> World
handleCollisions t w = let walls = shape <$> scenery w
                           bats = shape <$> paddle <$> players w
                        in w { players = restrictBats <$> players w
                             , ball = foldl doCollision (ball w) (walls ++ bats) }
          where
          restrictBats p = p { paddle = foldl paddleBlockCollision (paddle p) (scenery w) }

instance Updatable World where
  listen event world = world { players = listen event <$> players world }
  update t world = foldl (\w f -> f t w) world [
         updatePlayers,
         gravitate 400,
         integrate,
         checkForScore,
         handleEvents,
         handleCollisions
         ]
