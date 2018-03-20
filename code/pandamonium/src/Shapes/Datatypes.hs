module Shapes.Datatypes where

import Graphics.Gloss (Vector)

data Shape = Rectangle Float Float Float Float | Circle Vector Float deriving (Show, Eq)

class Shaped t where
  shape :: t -> Shape

data DeRectangle = DeRectangle Float Float Float Float deriving (Show, Eq)
data DeCircle = DeCircle Vector Float deriving (Show, Eq)

deconstruct :: Shape -> Either DeRectangle DeCircle
deconstruct (Rectangle l r t b) = Left $ DeRectangle l r t b
deconstruct (Circle c r) = Right $ DeCircle c r

construct :: Either DeRectangle DeCircle -> Shape
construct (Left (DeRectangle l r t b)) = Rectangle l r t b
construct (Right (DeCircle c r)) = Circle c r
