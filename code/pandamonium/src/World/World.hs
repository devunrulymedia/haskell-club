{-# LANGUAGE TemplateHaskell #-}

module World.World where

import Control.Lens
import Control.Arrow
import System.Exit

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game

import Entities.Ball
import Entities.Block
import Entities.Paddle
import Entities.Player
import Entities.Jumpman

import World.GameEvent
import Shapes.Shape
import Renderable
import Updatable

data World = World
                { _scenery :: [ Block ]
                , _events :: [ GameEvent ]
                , _jumpman :: Jumpman
                }

makeLenses ''World

instance IORenderable World where
  iorender world = pure $ Pictures $
                   (render <$> world ^. scenery) ++
                   [render $ world ^. jumpman]

gravitate :: Float -> Float -> World -> World
gravitate g t = jumpman %~ applyImpulse (0, -(g * t))

integrate :: Float -> World -> World
integrate t = jumpman %~ applyVelocity t

bounce :: (Movable a, Moving a, Shaped a, Shaped b) => Float -> a -> b -> a
bounce el a b = maybe a bounce' (shape b !!> shape a) where
  bounce' pushout = move offset (applyImpulse reflected_vel a) where
    vel           = velocity a
    unit_push     = normalizeV pushout
    offset        = mulSV (1 + el) pushout
    normal_proj   = (1 + el) * (vel `dotV` unit_push)
    reflected_vel = negate $ mulSV normal_proj unit_push

handleCollisions :: World -> World
handleCollisions w = jumpman %~ flip (foldl $ bounce 0) (w ^. scenery) $ w

exitOnEscape :: Event -> World -> IO World
exitOnEscape (EventKey key _ _ _) w = if key == Char 'q'
  then do exitSuccess
          return w
  else return w
exitOnEscape _ w = return w

instance IOUpdatable World where
  iolisten event world = return world
               <&> jumpman %~ listen event
               -- <&> (players %~ map (listen event))
               >>= exitOnEscape event
  ioupdate t world = return world
           <&> jumpman %~ update t
           <&> gravitate 400 t
           <&> integrate t
           -- <&> checkForScore
           -- <&> handleEvents
           <&> handleCollisions
