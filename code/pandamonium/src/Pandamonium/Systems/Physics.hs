module Pandamonium.Systems.Physics where

import Graphics.Gloss.Data.Vector
import Common.Redux
import Common.Shapes.Shape
import Pandamonium.Game.GameEvent

gravity :: Float
gravity = 1800

gravitate :: Moving a => Float -> a -> a
gravitate t = applyImpulse (0, -(gravity * t))

integrate :: Moving a => Float -> a -> a
integrate t = applyVelocity t
