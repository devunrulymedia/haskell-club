module Balls.Entities.Entities where

import Graphics.Gloss (Vector, Color, white)

import Common.Components.Entity
import Common.Components.Position
import Common.Components.Physics
import Common.Components.Renderer

import Common.Shapes.Shape

ball :: Vector -> Float -> Float -> Color -> Entity
ball pos mass radius col = entity
                       <-+ Position pos
                       <-+ Velocity (0, 0)
                       <-+ Acceleration (0, -1800)
                       <-+ circle (0, 0) radius
                       <-+ col
                       <-+ Mass mass

block :: Shape -> Entity
block shape = entity
          <-+ shape
          <-+ white
          <-+ Immovable
