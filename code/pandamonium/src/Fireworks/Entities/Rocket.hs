module Fireworks.Entities.Rocket where

import Graphics.Gloss (yellow)

import Common.Shapes.Shape (circle)
import Common.Components.Renderer
import Common.Components.Acceleration
import Common.Components.Position
import Common.Components.Velocity
import Common.Components.Components

rocket :: Components
rocket = components
     <-+ circle (0, 0) 20
     <-+ yellow
     <-+ Position 0 (-500)
     <-+ Velocity 0 0
     <-+ Acceleration 0 200
     <-+ coloredShape
