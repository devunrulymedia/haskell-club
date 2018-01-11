module Ball where

import Shape
import Vector
import Renderable
import Graphics.Gloss

data Ball = Ball {
  pos :: Vector.Vector,
  velocity :: Vector.Vector
} deriving (Show, Eq)

instance Renderable Ball where
  render ball = color yellow $ render Shape.Circle { centre = pos ball, radius = 10 }
