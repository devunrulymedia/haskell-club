{-# LANGUAGE DeriveAnyClass #-}

module Bomberman.Bomb where

import Graphics.Gloss (blue)
import Control.Monad.Trans
import Data.ConstrainedDynamic

import Common.Redux
import Common.Timer
import Common.Components
import Common.Shapes.Shape

data IsBomb = IsBomb deriving Component
data DropBomb = DropBomb Owner Float Float deriving ReduxEvent

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
                      <-+ onSpawn (Await 3 (toDyn (Destroy entityId)))
                      <-+ IsBomb
                      <-+ blue

spawnBomb :: DropBomb -> a -> IOEvents a
spawnBomb (DropBomb owner x y) a = do
  let (x', y') = alignToGrid (64, 64) (128, 128) (x, y)
  spawnWithId $ bomb owner x' y'
  return a
--
-- onBombSpawned :: Spawned -> a -> IOEvents a
-- onBombSpawned (Spawned entity) a = case (extract entity, extract entity) of
--   (Just IsBomb, Just entityId) -> do
--     liftIO $ putStrLn "Bomb spawned"
--     awaitEvent 3 (Destroy entityId)
--     return a
--   (_, _)  -> return a

bombRedux :: Redux World
bombRedux = Redux
  { updater  = noOp
  , listener = noOp
  , reducer  = composeHandler [ focusM spawnBomb ]
  }
