{-# LANGUAGE TemplateHaskell #-}

module Fireworks.World where

import Control.Lens
import Graphics.Gloss (Picture (Pictures))

import Common.Redux
import Common.Renderable
import Common.Components.Components
import Common.Components.Velocity
import Common.Components.Acceleration
import Common.Components.Renderer

import Fireworks.Entities.Rocket

data World = World
  { _entities :: [ Components ]
  }

makeLenses ''World

world :: World
world = World [ rocket ]

instance Renderable World where
  render world = Pictures $ render <$> (world ^. entities)

updateFireworks :: Float -> Components -> Events Components
updateFireworks time world = return world
                         <&> update applyAcceleration time
                         <&> update applyVelocity time

entityRedux :: Redux Components
entityRedux = noOpRedux { updater = updateFireworks}

fireworksRedux :: Redux World
fireworksRedux = connect (onAll entityRedux) entities
