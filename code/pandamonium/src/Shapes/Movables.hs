module Shapes.Movables where

import Graphics.Gloss.Data.Vector

class Movable a where
  move :: Vector -> a -> a

class Movable a => Moving a where
  velocity :: a -> Vector
  applyImpulse :: Vector -> a -> a
  applyVelocity :: Float -> a -> a
  applyVelocity t a = move (mulSV t (velocity a)) a
