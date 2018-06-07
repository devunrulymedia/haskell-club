module Shapes.Datatypes where

import Graphics.Gloss (Vector)

data Shape = Rectangle Float Float Float Float | Circle Vector Float deriving (Show, Eq)

rectangle :: Float -> Float -> Float -> Float -> Shape
rectangle l r t b = Rectangle (min l r) (max l r) (max t b) (min t b)

class Shaped t where
  shape :: t -> Shape
