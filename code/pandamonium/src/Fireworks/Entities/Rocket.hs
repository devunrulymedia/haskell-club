{-# LANGUAGE DeriveAnyClass #-}

module Fireworks.Entities.Rocket where

import Graphics.Gloss (Vector, Color, yellow)
import Data.Maybe
import Control.Lens ((<&>))

import Common.Redux
import Common.Shapes.Shape (circle)
import Common.Components.Renderer
import Common.Components.Lifecycle
import Common.Components.Position
import Common.Components.Entity

import Fireworks.Entities.Sparkle

data Fuel = Fuel Float deriving Component

data Explosion = Explosion Vector Color deriving ReduxEvent

data LaunchRocket = LaunchRocket Vector Color deriving ReduxEvent

rocket :: EntityId -> Entity
rocket = entity
     <-: circle (0, 0) 20
     <-: Velocity (0, 0)
     <-: Acceleration (0, 200)
     <-: Fuel 2

burn :: Float -> Entity -> Entity
burn t e = update1 burn' t e where
  burn' :: Float -> Fuel -> Fuel
  burn' t (Fuel f) = Fuel (f - t)

explode' :: Float -> Entity -> Maybe (Events Entity)
explode' t e = do
  (Position p) <- extract e
  colour <- extract e
  (Fuel fuel) <- extract e
  if fuel < 0
    then Just $ do
      destroy e
      fireEvent (Explosion p colour)
      return e
    else Nothing

explode :: Float -> Entity -> Events Entity
explode t e = fromMaybe (return e) (explode' t e)

updateRocket :: Float -> Entity -> Events Entity
updateRocket t e = return e
               <&> burn t
               >>= explode t

launch :: LaunchRocket -> a -> IOEvents a
launch (LaunchRocket p col) a = do
  spawn (rocket <-: Position p <-: col)
  return a

burst :: Explosion -> a -> IOEvents a
burst (Explosion p color) a = do
  traverse spawnSparkle (ring 15 500)
  return a where
    spawnSparkle vel = spawn $ sparkle (Position p) vel color

-- number of particles, how fast they're moving, yields their radial velocities
ring :: Float -> Float -> [ Velocity ]
ring n v = vel <$> [1..n] where
  vel x = let angle = 2 * pi * x / n
           in Velocity (v * cos angle, v * sin angle)

reduceRocket :: DynEvent -> [ Entity ] -> IOEvents [ Entity ]
reduceRocket d e = return e
               >>= (focusM launch) d
               >>= (focusM burst) d


rocketRedux :: Redux [ Entity ]
rocketRedux = Redux
  { updater  = lensing traverse updateRocket
  , listener = noOp
  , reducer  = reduceRocket
  }
