module Shapes.Shape where

import Movable
import Graphics.Gloss (translate, rectangleSolid, circleSolid, Vector)
import Graphics.Gloss.Data.Vector

data Rectangle = Rectangle Float Float Float Float deriving (Show, Eq)
data Circle = Circle Vector Float deriving (Show, Eq)

data Shape = Rect Rectangle | Circ Circle deriving (Show, Eq)

class Shaped t where
  shape :: t -> Shape
