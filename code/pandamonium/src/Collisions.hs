module Collisions where

import Graphics.Gloss (Vector)

infixl 2 !!!
infixl 2 !!>

class Collides a where
  (!!!) :: a -> a -> Bool
  (!!>) :: a -> a -> Maybe Vector
