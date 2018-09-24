module Common.Components.Velocity where

import Common.Components.Position

data Velocity = Velocity Float Float

applyVelocity :: Float -> Velocity -> Position -> Position
applyVelocity t (Velocity dx dy) (Position x y) = Position (x + t * dx) (y + t * dy)
