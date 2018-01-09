module Entity where

import Shape
import Graphics.Gloss

data Entity = Entity { pos :: Vector , shape :: Shape, color :: Color } deriving (Show, Eq)
