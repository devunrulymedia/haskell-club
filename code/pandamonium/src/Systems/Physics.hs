module Systems.Physics where

import Shapes.Movables

gravity :: Float
gravity = 1800

gravitate :: Moving a => Float -> a -> a
gravitate t = applyImpulse (0, -(gravity * t))

integrate :: Moving a => Float -> a -> a
integrate t = applyVelocity t
