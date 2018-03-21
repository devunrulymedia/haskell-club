{-# LANGUAGE TemplateHaskell #-}

module World.World where

import Control.Lens

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game

import Entities.Ball
import Entities.Block
import Entities.Paddle
import Entities.Player

import World.GameEvent
import Shapes.Shape
import Renderable
import Updatable

data World = World
                { _scenery :: [ Block ]
                , _players :: [Player]
                , _ball :: Ball
                , _initialBall :: Ball
                , _events :: [ GameEvent ]
                }

makeLenses ''World

instance Renderable World where
  render world = Pictures $
                  (render <$> _scenery world) ++
                  (render <$> _players world) ++
                  [(render $ _ball world)]

gravitate :: Float -> Float -> World -> World
gravitate g t = over ball (applyImpulse (0, -(g * t)))

integrate :: Float -> World -> World
integrate t w@World { _ball = (Ball pos vel pic) } = w { _ball = Ball (pos + mulSV t vel) vel pic }

updatePlayers :: Float -> World -> World
updatePlayers t world = world { _players = update t <$> _players world }

checkForScore :: Float -> World -> World
checkForScore t world = let newEvents = _players world >>= checkScore'
                         in world { _events = newEvents ++ _events world } where
  checkScore' :: Player -> [ GameEvent ]
  checkScore' player = if shape (_ball world) !!! shape (endzone player)
    then [ PointScored $ playerNumber player ]
    else []

resetBall :: GameEvent -> World -> World
resetBall (PointScored _) world = world { _ball = _initialBall world }

handleEvents :: Float -> World -> World
handleEvents t w = (foldl (flip handleEvent) w (_events w)) { _events = [] }

instance GameEvents World where
  handleEvent e world = resetBall e world { _players = handleEvent e <$> _players world }

doCollision :: (Movable a, Moving a, Shaped a) => a -> Shape -> a
doCollision a wall = maybe a handleCollision (wall !!> shape a) where
  handleCollision pushout = move offset (applyImpulse reflected_vel a) where
    vel           = velocity a
    unit_push     = normalizeV pushout
    offset        = mulSV 2 pushout
    normal_proj   = 2 * (vel `dotV` unit_push)
    reflected_vel = negate $ mulSV normal_proj unit_push

paddleBlockCollision :: Paddle -> Block -> Paddle
paddleBlockCollision paddle block = maybe paddle (flip move $ paddle) (shape block !!> shape paddle)

handleCollisions :: Float -> World -> World
handleCollisions t w = let walls = shape <$> _scenery w
                           bats = shape <$> paddle <$> _players w
                        in w { _players = restrictBats <$> _players w
                             , _ball = foldl doCollision (_ball w) (walls ++ bats) }
          where
          restrictBats p = p { paddle = foldl paddleBlockCollision (paddle p) (_scenery w) }

instance Updatable World where
  listen event world = world { _players = listen event <$> _players world }
  update t world = foldl (\w f -> f t w) world [
         updatePlayers,
         gravitate 400,
         integrate,
         checkForScore,
         handleEvents,
         handleCollisions
         ]
