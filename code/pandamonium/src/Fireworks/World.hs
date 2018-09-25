{-# LANGUAGE TemplateHaskell #-}

module Fireworks.World where

import Control.Lens
import Graphics.Gloss (Picture (Pictures))

import Common.Redux
import Common.Renderable
import Common.Components.Entity
import Common.Components.Position
import Common.Components.Renderer

import Fireworks.Entities.Rocket

data World = World
  { _entities :: [ Entity ]
  }

makeLenses ''World

world :: World
world = World [ rocket ]

instance Renderable World where
  render world = Pictures $ render <$> (world ^. entities)

updateFireworks :: Float -> Entity -> Events Entity
updateFireworks time world = return world
                         <&> update applyAcceleration time
                         <&> update applyVelocity time

entityRedux :: Redux Entity
entityRedux = noOpRedux { updater = updateFireworks}

fireworksRedux :: Redux World
fireworksRedux = connect (onAll entityRedux) entities
