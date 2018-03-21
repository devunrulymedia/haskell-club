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
                , _players :: [ Player ]
                , _ball :: Ball
                , _initialBall :: Ball
                , _events :: [ GameEvent ]
                }

makeLenses ''World

instance Renderable World where
  render world = Pictures $
                  (render <$> world ^. scenery) ++
                  (render <$> world ^. players) ++
                  [(render $ world ^. ball)]

gravitate :: Float -> Float -> World -> World
gravitate g t = ball %~ applyImpulse (0, -(g * t))

integrate :: Float -> World -> World
integrate t = ball %~ applyVelocity t

updatePlayers :: Float -> World -> World
updatePlayers t = players %~ (map $ update t)

checkForScore :: Float -> World -> World
checkForScore t world = let newEvents = world ^. players >>= checkScore'
                         in events %~ (newEvents ++) $ world where
  checkScore' :: Player -> [ GameEvent ]
  checkScore' player = if shape (world ^. ball) !!! shape (player ^. endzone)
    then [ PointScored $ player ^. playerNumber ]
    else []

resetBall :: GameEvent -> World -> World
resetBall (PointScored _) world = ball .~ (world ^. initialBall) $ world

handleEvents :: Float -> World -> World
handleEvents t w = events .~ [] $ (foldl (flip handleEvent) w (w ^. events))

instance GameEvents World where
  handleEvent e world = let a = players %~ (map (handleEvent e)) $ world
                         in resetBall e a

doCollision :: (Movable a, Moving a, Shaped a) => a -> Shape -> a
doCollision a wall = maybe a handleCollision (wall !!> shape a) where
  handleCollision pushout = move offset (applyImpulse reflected_vel a) where
    vel           = velocity a
    unit_push     = normalizeV pushout
    offset        = mulSV 2 pushout
    normal_proj   = 2 * (vel `dotV` unit_push)
    reflected_vel = negate $ mulSV normal_proj unit_push

paddleBlockCollision :: (Movable a, Shaped a, Shaped b) => a -> b -> a
paddleBlockCollision a b = maybe a (flip move $ a) (shape b !!> shape a)

handleCollisions :: Float -> World -> World
handleCollisions t w = let walls = shape <$> _scenery w
                           bats = shape <$> (view paddle) <$> w ^. players
                        in w { _players = restrictBats <$> _players w
                             , _ball = foldl doCollision (_ball w) (walls ++ bats) }
          where
          restrictBats p = p { _paddle = foldl paddleBlockCollision (p ^. paddle) (w ^. scenery) }

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
