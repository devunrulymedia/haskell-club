{-# LANGUAGE TemplateHaskell #-}

module Fireworks.World where

import Control.Lens
import Graphics.Gloss (Picture (Pictures), yellow, green)
import Data.ConstrainedDynamic

import Common.Redux
import Common.Renderable
import Common.Timer
import Common.Components.Entity
import Common.Components.Lifecycle
import Common.Components.Position
import Common.Components.Renderer

import Fireworks.Entities.Rocket

data World = World
  { _entities :: [ Entity ]
  , _entityId :: EntityId
  }

makeLenses ''World

emptyWorld :: World
emptyWorld = World [] (EntityId 0)

instance Renderable World where
  render world = Pictures $ draw spritesAndShapes <$> (world ^. entities)

updateFireworks :: Float -> Entity -> Events Entity
updateFireworks time world = return world
                         <&> update2 applyAcceleration time
                         <&> update2 applyVelocity time

entityRedux :: Redux Entity
entityRedux = noOpRedux { updater = updateFireworks }

fireworksRedux :: Redux World
fireworksRedux = compose
  [ connect (onAll entityRedux) entities
  , connect rocketRedux entities
  , lifecycle entities entityId
  ]