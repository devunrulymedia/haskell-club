{-# LANGUAGE DeriveAnyClass #-}

module Fireworks.Entities.Sparkle where

import Graphics.Gloss (Color)
import Data.ConstrainedDynamic

import Common.Components.Position
import Common.Components.Renderer
import Common.Components.Lifecycle
import Common.Components.Entity
import Common.Shapes.Shape
import Common.Redux
import Common.Timer

sparkle :: Position -> Velocity -> Color -> EntityId -> Entity
sparkle pos vel col entityId = entity
                           <-+ pos
                           <-+ vel
                           <-+ col
                           <-+ circle (0, 0) 10
                           <-+ OnSpawn (awaitEvent 2 $ Destroy entityId)
                           <-+ Acceleration (0, -300)
