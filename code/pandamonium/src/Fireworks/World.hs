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
import Common.Components.World

import Fireworks.Entities.Rocket
import Fireworks.Entities.Sparkle

updateFireworks :: Float -> Entity -> Events Entity
updateFireworks time world = return world
                         <&> update2 applyAcc time
                         <&> update2 applyVel time

entityRedux :: Redux Entity
entityRedux = Redux
  { updater  = updateFireworks
  , listener = noOp
  , reducer  = noOp }

fireworksRedux :: Redux World
fireworksRedux = compose
  [ connect (onAll entityRedux) entities
  , connect rocketRedux entities
  , lifecycle
  ]
