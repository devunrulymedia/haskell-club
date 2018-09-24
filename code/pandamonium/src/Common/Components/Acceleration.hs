module Common.Components.Acceleration where

import Common.Components.Velocity

data Acceleration = Acceleration Float Float

applyAcceleration :: Float -> Acceleration -> Velocity -> Velocity
applyAcceleration t (Acceleration dx dy) (Velocity x y) = Velocity (x + t * dx) (y + t * dy)
