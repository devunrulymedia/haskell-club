module Fireworks.Entities.Panda where

import Graphics.Gloss (Picture)

import Common.Components.Entity
import Common.Components.Destroyer
import Common.Components.Position
import Common.Components.Renderer

panda :: Picture -> Entity
panda sprite = entity
           <-+ EntityId 3
           <-+ Position 400 0
           <-+ Sprite sprite
           <-+ Zoom 4
