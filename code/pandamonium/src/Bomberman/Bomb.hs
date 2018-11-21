{-# LANGUAGE DeriveAnyClass #-}

module Bomberman.Bomb where

import Graphics.Gloss (blue)

import Common.Redux
import Common.Components
import Common.Shapes.Shape

data DropBomb = DropBomb Owner Float Float deriving ReduxEvent

alignToGrid :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
alignToGrid (cx, cy) (sx, sy) (px, py) = (align cx sx px, align cy sy py) where
  align :: Float -> Float -> Float -> Float
  align c s p = let translatedToGridFrame = p - c
                    roundedToNearestPoint = (fromIntegral (round (translatedToGridFrame / s))) * s
                    translatedBackToOriginalFrame = roundedToNearestPoint + c
                 in translatedBackToOriginalFrame

bomb :: Owner -> Float -> Float -> Entity
bomb owner x y = entity
             <-+ owner
             <-+ Position (x, y)
             <-+ circle (0, 0) 40
             <-+ blue

dropBomb :: DropBomb -> a -> IOEvents a
dropBomb (DropBomb owner x y) a = do
  spawn $ bomb owner x y
  return a

bombRedux :: Redux World
bombRedux = Redux
  { updater  = noOp
  , listener = noOp
  , reducer  = focusM dropBomb
  }
