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

sparkle :: Position -> Velocity -> Color -> MkEntity
sparkle pos vel col = entity
                  <-: pos
                  <-: vel
                  <-: col
                  <-: circle (0, 0) 10
                  <-: OnSpawn (destroyIn 2)
                  <-: Acceleration (0, -300)
