module Common.Physics.Physics where

import Graphics.Gloss.Data.Vector
import Common.Shapes.Shape

class (Shaped a, Moving a) => Physics a where
  mass :: a -> Float
  elasticity :: a -> Float

gravity :: Float
gravity = 1800

gravitate :: Moving a => Float -> a -> a
gravitate t = applyImpulse (0, -(gravity * t))

integrate :: Moving a => Float -> a -> a
integrate t = applyVelocity t
