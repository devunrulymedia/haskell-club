module Shapes.Datatypes where

import Graphics.Gloss (Vector)

data Shape = Rectangle Float Float Float Float | Circle Vector Float deriving (Show, Eq)

class Shaped t where
  shape :: t -> Shape
