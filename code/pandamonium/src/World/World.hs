{-# LANGUAGE TemplateHaskell #-}

module World.World where

import Control.Lens
import Control.Arrow
import System.IO

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

instance IORenderable World where
  iorender world = pure $ Pictures $
                  (render <$> world ^. scenery) ++
                  (render <$> world ^. players) ++
                  [(render $ world ^. ball)]

gravitate :: Float -> Float -> World -> World
gravitate g t = ball %~ applyImpulse (0, -(g * t))

integrate :: Float -> World -> World
integrate t = ball %~ applyVelocity t

updatePlayers :: Float -> World -> World
updatePlayers t = players %~ (map $ update t)

checkForScore :: World -> World
checkForScore world = let newEvents = world ^. players >>= checkScore'
                       in events %~ (newEvents ++) $ world where
  checkScore' :: Player -> [ GameEvent ]
  checkScore' player = if shape (world ^. ball) !!! shape (player ^. endzone)
    then [ PointScored $ player ^. playerNumber ]
    else []

resetBall :: GameEvent -> World -> World
resetBall (PointScored _) world = ball .~ (world ^. initialBall) $ world

handleEvents :: World -> World
handleEvents w = events .~ [] $ (foldl (flip handleEvent) w (w ^. events))

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

handleCollisions :: World -> World
handleCollisions w = let walls = shape <$> w ^. scenery
                         bats = shape <$> (view paddle) <$> w ^. players
                      in players %~ (map restrictBats)
                         $ ball %~ flip (foldl doCollision) (walls ++ bats)
                         $ w where
          restrictBats = paddle %~ flip (foldl paddleBlockCollision) (w ^. scenery)

instance IOUpdatable World where
  iolisten event world = return $ (players %~ map (listen event)) world
  ioupdate t = updatePlayers t
           >>> gravitate 400 t
           >>> integrate t
           >>> checkForScore
           >>> handleEvents
           >>> handleCollisions 
           >>> return
