module Fireworks.Entities.Sparkle where

import Graphics.Gloss (Color)

import Common.Components.Position
import Common.Components.Renderer
import Common.Components.Lifecycle
import Common.Components.Entity
import Common.Shapes.Shape

sparkle :: Position -> Velocity -> Color -> Entity
sparkle pos vel col = entity
                  <-+ pos
                  <-+ vel
                  <-+ col
                  <-+ circle (0, 0) 10
                  <-+ Acceleration (0, -300)
                  <-+ Lifespan 2
