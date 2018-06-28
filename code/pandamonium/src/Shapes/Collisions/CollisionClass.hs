module Shapes.Collisions.CollisionClass where

import Graphics.Gloss (Vector)

infixl 2 !!!
infixl 2 !!>

class Collides a where
  (!!!) :: a -> a -> Bool
  (!!>) :: a -> a -> Maybe Vector

sqMagV :: Vector -> Float
sqMagV (x, y) = x * x + y * y
