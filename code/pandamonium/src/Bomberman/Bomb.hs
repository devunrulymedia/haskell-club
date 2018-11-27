{-# LANGUAGE DeriveAnyClass #-}

module Bomberman.Bomb where

import Graphics.Gloss (red, blue)
import Control.Monad.Trans
import Data.ConstrainedDynamic

import Common.Redux
import Common.Timer
import Common.Components
import Common.Shapes.Shape

data IsBomb = IsBomb deriving Component
data DropBomb = DropBomb Owner Float Float deriving ReduxEvent
data Exploded = Exploded Owner Float Float deriving ReduxEvent

alignToGrid :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
alignToGrid (cx, cy) (sx, sy) (px, py) = (align cx sx px, align cy sy py) where
  align :: Float -> Float -> Float -> Float
  align c s p = let translatedToGridFrame = p - c
                    roundedToNearestPoint = (fromIntegral (round (translatedToGridFrame / s))) * s
                    translatedBackToOriginalFrame = roundedToNearestPoint + c
                 in translatedBackToOriginalFrame

bomb :: Owner -> Float -> Float -> EntityId -> Entity
bomb owner x y entityId = entity
                      <-+ owner
                      <-+ Position (x, y)
                      <-+ circle (0, 0) 40
                      <-+ Immovable
                      <-+ MaxPush 2
                      <-+ OnSpawn (await 3 (Destroy entityId))
                      <-+ OnDestroy (toDyn $ Exploded owner x y)
                      <-+ IsBomb
                      <-+ blue

explosion :: Float -> Float -> EntityId -> Entity
explosion x y entityId = entity
                     <-+ Position (x, y)
                     <-+ circle (0, 0) 50
                     <-+ OnSpawn (await 1 (Destroy entityId))
                     <-+ red

explode :: Exploded -> a -> IOEvents a
explode (Exploded _ x y) a = do
  spawnWithId $ explosion x y
  spawnWithId $ explosion (x + 128) y
  spawnWithId $ explosion (x - 128) y
  spawnWithId $ explosion x (y + 128)
  spawnWithId $ explosion x (y - 128)
  return a

spawnBomb :: DropBomb -> a -> IOEvents a
spawnBomb (DropBomb owner x y) a = do
  let (x', y') = alignToGrid (64, 64) (128, 128) (x, y)
  spawnWithId $ bomb owner x' y'
  return a

bombRedux :: Redux World
bombRedux = Redux
  { updater  = noOp
  , listener = noOp
  , reducer  = composeHandler [ focusM spawnBomb, focusM explode ]
  }
