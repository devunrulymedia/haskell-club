{-# LANGUAGE TemplateHaskell #-}

module Balls.World.World where

import Control.Lens
import Graphics.Gloss ( Picture (Pictures), white, red, blue, yellow, green )

import Common.Renderable
import Common.Redux
import Common.Relationship

import Common.Components.Entity
import Common.Components.Physics
import Common.Components.Lifecycle
import Common.Components.Renderer

import Common.Shapes.Shape

import Balls.Entities.Entities

data World = World
  { _entities :: [ Entity ]
  , _entityId :: EntityId
  }

makeLenses ''World

instance Renderable World where
  render world = Pictures $ draw spritesAndShapes <$> (world ^. entities)

ballsRedux :: Redux World
ballsRedux = compose
  [ connect physicsRedux entities
  , lifecycle entities entityId
  ]

initialiseWorld :: Events ()
initialiseWorld = do
  spawn $ block (rectangleV (-600, 400) (1200, 20))
  spawn $ block (rectangleV (-600, 100) (20, 300))
  spawn $ block (rectangleV (580, 100) (20, 300))
  spawn $ block (polygon [(-600, 100), (-580, 100), (-180, -300), (-200, -300)])
  spawn $ block (rectangleV (-200, -600) (20, 300))
  spawn $ block (rectangleV (-200, -600) (400, 20))
  spawn $ block (rectangleV (180, -600) (20, 300))
  spawn $ block (polygon [(600, 100), (580, 100), (180, -300), (200, -300)])

  spawn $ ball (0, 0) 1 30 red
  spawn $ ball (10, 75) 2 40 blue
  spawn $ ball (100, 25) 4 60 green
  spawn $ ball (-200, 25) 1 25 yellow

world :: World
world = World [] (EntityId 0)
