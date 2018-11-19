{-# LANGUAGE DeriveAnyClass #-}

module Fireworks.Entities.Sparkle where

import Graphics.Gloss (Color)

import Common.Components.Position
import Common.Components.Renderer
import Common.Components.Lifecycle
import Common.Components.Entity
import Common.Shapes.Shape
import Common.Redux
import Common.Timer

data IsSparkle = IsSparkle deriving Component

sparkle :: Position -> Velocity -> Color -> Entity
sparkle pos vel col = entity
                  <-+ pos
                  <-+ vel
                  <-+ col
                  <-+ circle (0, 0) 10
                  <-+ IsSparkle
                  <-+ Acceleration (0, -300)

reduceSparkle :: Spawned -> a -> IOEvents a
reduceSparkle (Spawned ent) a = do
  case (extract ent, extract ent) of
    (Nothing, _) ->
      return a
    (Just IsSparkle, Just entityId) -> do
      awaitEvent 2 (Destroy entityId)
      return a
