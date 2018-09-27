module Fireworks.Entities.Rocket where

import Graphics.Gloss (yellow)

import Common.Shapes.Shape (circle)
import Common.Components.Renderer
import Common.Components.Position
import Common.Components.Entity

rocket :: Entity
rocket = entity
     <-+ circle (0, 0) 20
     <-+ yellow
     <-+ Position 0 (-500)
     <-+ Velocity 0 0
     <-+ Acceleration 0 200
