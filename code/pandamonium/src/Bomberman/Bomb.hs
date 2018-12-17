{-# LANGUAGE DeriveAnyClass #-}

module Bomberman.Bomb where

import Data.Maybe
import Graphics.Gloss (red, blue)

import Common.Monad
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
bomb owner x y entityId = entity entityId
                      <-+ owner
                      <-+ Position (x, y)
                      <-+ circle (0, 0) 40
                      <-+ Immovable
                      <-+ MaxPush 2
                      <-+ onSpawn (awaitEvent 3 . destroy')
                      <-+ onDestroy exploded
                      <-+ IsBomb
                      <-+ blue

exploded :: Entity -> IOEvents ()
exploded entity = whenJust $ do
  owner <- extract entity
  (Position (x, y)) <- extract entity
  return $ fireEvent $ Exploded owner x y

explosion :: Float -> Float -> EntityId -> Entity
explosion x y entityId = entity entityId
                     <-+ Position (x, y)
                     <-+ circle (0, 0) 50
                     <-+ onSpawn (awaitEvent 3 . destroy')
                     <-+ red

explode :: Exploded -> a -> IOEvents a
explode (Exploded _ x y) a = do
  spawn $ explosion x y
  spawn $ explosion (x + 128) y
  spawn $ explosion (x - 128) y
  spawn $ explosion x (y + 128)
  spawn $ explosion x (y - 128)
  return a

spawnBomb :: DropBomb -> a -> IOEvents a
spawnBomb (DropBomb owner x y) a = do
  let (x', y') = alignToGrid (64, 64) (128, 128) (x, y)
  spawn $ bomb owner x' y'
  return a

bombRedux :: Redux World
bombRedux = Redux
  { updater  = noOp
  , listener = noOp
  , reducer  = composeHandler [ focusM spawnBomb, focusM explode ]
  }
