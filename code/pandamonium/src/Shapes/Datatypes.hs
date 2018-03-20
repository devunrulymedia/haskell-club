module Shapes.Datatypes where

import Graphics.Gloss (Vector)

data Rectangle = Rectangle Float Float Float Float deriving (Show, Eq)
data Circle = Circle Vector Float deriving (Show, Eq)

data Shape = Rect Rectangle | Circ Circle deriving (Show, Eq)

class Shaped t where
  shape :: t -> Shape
