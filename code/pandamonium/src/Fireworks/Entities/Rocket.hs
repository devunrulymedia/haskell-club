{-# LANGUAGE DeriveAnyClass #-}

module Fireworks.Entities.Rocket where

import Graphics.Gloss (Color, yellow)
import Data.Maybe
import Control.Lens ((<&>))
import Debug.Trace

import Common.Redux
import Common.Shapes.Shape (circle)
import Common.Components.Renderer
import Common.Components.Lifecycle
import Common.Components.Position
import Common.Components.Entity

data Fuel = Fuel Float deriving Component

data Explosion = Explosion Float Float Color deriving ReduxEvent

rocket :: Entity
rocket = entity
     <-+ EntityId 1001 -- this should actually happen via the lifecycle but whatever
     <-+ circle (0, 0) 20
     <-+ yellow
     <-+ Position 0 (-500)
     <-+ Velocity 0 0
     <-+ Acceleration 0 200
     <-+ Fuel 2

burn :: Float -> Entity -> Entity
burn t e = update1 burn' t e where
  burn' :: Float -> Fuel -> Fuel
  burn' t (Fuel f) = Fuel (f - t)

explode' :: Float -> Entity -> Maybe (Events Entity)
explode' t e = do
  (Position x y) <- from e
  colour <- from e
  (Fuel fuel) <- from e
  if fuel < 0
    then Just $ do
      trace "Boom!" $ destroy e
      fireEvent (Explosion x y colour)
      return e
    else Nothing

explode :: Float -> Entity -> Events Entity
explode t e = fromMaybe (return e) (explode' t e)

updateRocket :: Float -> Entity -> Events Entity
updateRocket t e = return e
               <&> burn t
               >>= explode t

rocketRedux :: Redux Entity
rocketRedux = noOpRedux { updater = updateRocket }
