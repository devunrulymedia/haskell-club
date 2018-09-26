{-# LANGUAGE TemplateHaskell #-}

module Fireworks.World where

import Control.Lens
import Graphics.Gloss (Picture (Pictures))
import Data.ConstrainedDynamic

import Common.Redux
import Common.Renderable
import Common.Timer
import Common.Components.Entity
import Common.Components.Lifecycle
import Common.Components.Position
import Common.Components.Renderer
import Common.Components.Trigger

import Fireworks.Assets
import Fireworks.Entities.Rocket
import Fireworks.Entities.Panda

data World = World
  { _entities :: [ Entity ]
  , _timer :: Timer
  , _entityId :: EntityId
  }

makeLenses ''World

world :: Assets -> World
world assets = World
  [ rocket
  , delayedSpawn 3 (panda (assets ^. pandaSprite))
  ]
  newTimer
  (EntityId 0)

instance Renderable World where
  render world = Pictures $ draw spritesAndShapes <$> (world ^. entities)

updateFireworks :: Float -> Entity -> Events Entity
updateFireworks time world = return world
                         <&> update applyAcceleration time
                         <&> update applyVelocity time

entityRedux :: Redux Entity
entityRedux = noOpRedux { updater = updateFireworks }

fireworksRedux :: Redux World
fireworksRedux = compose
  [ connect (onAll entityRedux) entities
  , connect (onAll triggerRedux) entities
  , lifecycle entities entityId
  , connect timerRedux timer
  ]
