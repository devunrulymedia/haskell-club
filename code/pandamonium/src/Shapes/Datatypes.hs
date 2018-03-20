module Shapes.Datatypes where

import Graphics.Gloss (Vector)

data Rectangle = Rectangle Float Float Float Float deriving (Show, Eq)
data Circle = Circle Vector Float deriving (Show, Eq)

data Shape = Rect Float Float Float Float | Circ Vector Float deriving (Show, Eq)

deconstruct :: Shape -> Either Rectangle Circle
deconstruct (Rect l r t b) = Left $ Rectangle l r t b
deconstruct (Circ c r) = Right $ Circle c r

class Shaped t where
  shape :: t -> Shape
