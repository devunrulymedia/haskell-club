module Entity where

import Shape
import Graphics.Gloss

data Entity = Entity { x :: Float, y :: Float, shape :: Shape, color :: Color } deriving (Show, Eq)
