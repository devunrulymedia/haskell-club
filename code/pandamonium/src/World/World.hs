{-# LANGUAGE TemplateHaskell #-}

module World.World where

import Control.Lens
import Control.Arrow
import System.Exit
import Control.Monad.Writer
import Data.DList

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game

import Entities.Block
import Entities.Jumpman

import World.GameEvent
import Shapes.Shape
import Renderable
import Updatable
import Redux

data World = World
                { _scenery :: [ Block ]
                , _jumpman :: Jumpman
                }

makeLenses ''World

type Listener = Event -> World -> Events GameEvent World
type Updater  = Float -> World -> Events GameEvent World
type Reducer  = GameEvent -> World -> IO World

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

listenForQuit :: Listener
listenForQuit (EventKey (Char 'q') _ _ _ ) w = do fireEvent Quit; return w
listenForQuit _ w = return w

listenWorld :: Listener
listenWorld e w = return w
              <&> jumpman %~ collectEvents e
              >>= listenForQuit e

quit :: Reducer
quit Quit w = do exitSuccess; return w
quit _ w    = return w

reduceWorld :: Reducer
reduceWorld = quit

updateWorld :: Updater
updateWorld t w = return w
              <&> jumpman %~ update t
              <&> gravitate 1800 t
              <&> integrate t
              <&> handleCollisions

redux :: Redux World GameEvent
redux = Redux
  { reducer  = reduceWorld
  , listener = listenWorld
  , updater  = updateWorld
  }

instance IOUpdatable World where
  iolisten = reduxListen redux
  ioupdate = reduxUpdate redux
