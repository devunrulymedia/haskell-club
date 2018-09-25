module Fireworks.Entities.Panda where

import Graphics.Gloss (Picture)

import Common.Components.Entity
import Common.Components.Position
import Common.Components.Renderer

panda :: Picture -> Entity
panda sprite = entity
           <-+ Position 400 0
           <-+ Sprite sprite
           <-+ Zoom 4
