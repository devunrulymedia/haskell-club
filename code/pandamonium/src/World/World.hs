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


listenWorld :: Event -> World -> [GameEvent]
listenWorld (EventKey (Char 'q') _ _ _ ) w = [Quit]
listenWorld _ _ = []

quit :: World -> GameEvent -> IO World
quit w Quit = do exitSuccess; return w
quit w _ = return w

reduceWorld :: World -> GameEvent -> IO World
reduceWorld w e = foldM (\w r -> r w e) w
  [ quit
  ]

updateWorld :: World -> Float -> Writer (DList GameEvent) World
updateWorld w t = return w
              <&> jumpman %~ update t
              <&> gravitate 1800 t
              <&> integrate t
              <&> handleCollisions

redux :: Redux World GameEvent
redux = Redux
  { reducer = reduceWorld
  , listener = listenWorld
  , updater = updateWorld
  }

instance IOUpdatable World where
  iolisten event world = return world
               <&> jumpman %~ collectEvents event
               >>= exitOnEscape event
  ioupdate = reduxUpdate redux
