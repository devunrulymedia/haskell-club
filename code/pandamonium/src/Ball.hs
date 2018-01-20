module Ball where

import Shape
import Renderable
import Graphics.Gloss

data Ball = Ball {
  pos :: Vector,
  velocity :: Vector
} deriving (Show, Eq)

instance Renderable Ball where
  render ball = color yellow $ render $ Shape.Circle (pos ball) 10
