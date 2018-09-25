module Common.Components.Position where

data Position = Position Float Float

data Velocity = Velocity Float Float

applyVelocity :: Float -> Velocity -> Position -> Position
applyVelocity t (Velocity dx dy) (Position x y) = Position (x + t * dx) (y + t * dy)

data Acceleration = Acceleration Float Float

applyAcceleration :: Float -> Acceleration -> Velocity -> Velocity
applyAcceleration t (Acceleration dx dy) (Velocity x y) = Velocity (x + t * dx) (y + t * dy)
