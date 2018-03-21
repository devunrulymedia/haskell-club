module Entities.Ball where

import Shapes.Shape
import Renderable
import Graphics.Gloss (color, yellow, translate, Vector, Picture)

data Ball = Ball Vector Vector Picture deriving (Show, Eq)

instance Shaped Ball where
  shape (Ball pos _ _) = Circle pos 10

instance Renderable Ball where
  render (Ball (x, y) vel pic) = color yellow $ translate x y pic

instance Movable Ball where
  move (Ball (x, y) vel pic) (dx, dy) = Ball (x + dx, y + dy) vel pic

instance Moving Ball where
  velocity (Ball _ v _) = v
  applyImpulse (Ball pos (x, y) pic) (dx, dy) = Ball pos (x + dx, y + dy) pic
