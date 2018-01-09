module World where

import Entity
import Block
import Renderable
import Graphics.Gloss

data World = World { scenery :: [ Block ], entities :: [ Entity ] } deriving (Show, Eq)

instance Renderable World where
  render world = Pictures $ render <$> scenery world
