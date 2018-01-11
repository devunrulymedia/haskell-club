module World where

import Ball
import Block
import Renderable
import Graphics.Gloss

data World = World { scenery :: [ Block ], ball :: Ball } deriving (Show, Eq)

instance Renderable World where
  render world = Pictures $ (render <$> scenery world) ++ [(render $ ball world)]
