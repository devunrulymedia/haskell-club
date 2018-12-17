{-# LANGUAGE TemplateHaskell #-}

module Common.Components.World where

import Control.Lens
import Graphics.Gloss (Picture (Pictures))

import Common.Renderable
import Common.Components.Entity
import Common.Components.Renderer

data World = World
  { _entities     :: [ Entity ]
  , _nextEntityId :: EntityId
  , _renderer     :: Renderer
  }

makeLenses ''World

instance Renderable World where
  render world = Pictures $ draw (world ^. renderer) <$> (world ^. entities)

newWorld :: Renderer -> World
newWorld renderer = World [] 0 renderer
