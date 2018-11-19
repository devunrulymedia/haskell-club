{-# LANGUAGE DeriveAnyClass #-}

module Bomberman.Bomb where

import Common.Redux
import Common.Components
import Common.Components

data DropBomb = DropBomb Owner Float Float deriving ReduxEvent

data Owner = Owner EntityId deriving Component

alignToGrid :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
alignToGrid (cx, cy) (sx, sy) (px, py) = (slot cx sx px, slot cy sy py) where
  slot :: Float -> Float -> Float -> Float
  slot c s p = (fromIntegral (round ((p - c) / s))) * s + c

bomb :: EntityId -> Float -> Float -> Entity
bomb owner x y = entity
             <-+ Owner owner
             <-+ Position (x, y)
