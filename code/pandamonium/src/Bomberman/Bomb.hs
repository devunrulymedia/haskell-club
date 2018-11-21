{-# LANGUAGE DeriveAnyClass #-}

module Bomberman.Bomb where

import Common.Redux
import Common.Components
import Common.Components

data DropBomb = DropBomb Owner Float Float deriving ReduxEvent

alignToGrid :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
alignToGrid (cx, cy) (sx, sy) (px, py) = (align cx sx px, align cy sy py) where
  align :: Float -> Float -> Float -> Float
  align c s p = let translatedToGridFrame = p - c
                    roundedToNearestPoint = (fromIntegral (round (translatedToGridFrame / s))) * s
                    translatedBackToOriginalFrame = roundedToNearestPoint + c
                 in translatedBackToOriginalFrame

bomb :: EntityId -> Float -> Float -> Entity
bomb owner x y = entity
             <-+ Owner owner
             <-+ Position (x, y)
