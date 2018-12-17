module Panda2.Entities.Block where

import Graphics.Gloss (white)

import Common.Components
import Common.Shapes.Shape

block :: Shape -> EntityId -> Entity
block shape = entity
          <-: shape
          <-: white
