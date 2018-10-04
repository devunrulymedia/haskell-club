{-# LANGUAGE DeriveAnyClass #-}

module Common.Components.Position where

import Common.Components.Entity
import Graphics.Gloss (Vector)

data Position = Position Vector deriving Component

data Velocity = Velocity Vector deriving Component

data Acceleration = Acceleration Vector deriving Component

applyVelocity :: Float -> Velocity -> Position -> Position
applyVelocity t (Velocity (dx, dy)) (Position (x, y)) = Position (x + t * dx, y + t * dy)

applyAcceleration :: Float -> Acceleration -> Velocity -> Velocity
applyAcceleration t (Acceleration (dx, dy)) (Velocity (x, y)) = Velocity (x + t * dx, y + t * dy)

onPosition :: (Vector -> Vector) -> Position -> Position
onPosition f (Position v) = Position $ f v

onVelocity :: (Vector -> Vector) -> Velocity -> Velocity
onVelocity f (Velocity v) = Velocity $ f v

onAcceleration :: (Vector -> Vector) -> Acceleration -> Acceleration 
onAcceleration f (Acceleration v) = Acceleration $ f v
