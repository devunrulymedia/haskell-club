{-# LANGUAGE DeriveAnyClass #-}

module Common.Components.Position where

import Common.Components.Entity

data Position = Position Float Float deriving Component

data Velocity = Velocity Float Float deriving Component

data Acceleration = Acceleration Float Float deriving Component

applyVelocity :: Float -> Velocity -> Position -> Position
applyVelocity t (Velocity dx dy) (Position x y) = Position (x + t * dx) (y + t * dy)

applyAcceleration :: Float -> Acceleration -> Velocity -> Velocity
applyAcceleration t (Acceleration dx dy) (Velocity x y) = Velocity (x + t * dx) (y + t * dy)
