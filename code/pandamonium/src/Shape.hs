module Shape where

import Vector
import Renderable
import Graphics.Gloss (translate, circleSolid, rectangleSolid)

data Shape = Rectangle { left :: Float, right :: Float, top :: Float, bottom :: Float }
           | Circle { centre :: Vector, radius :: Float } deriving (Show, Eq)

-- checks if two shapes collide, returning a boolean
infixl 2 !!!
(!!!) :: Shape -> Shape -> Bool
a@Rectangle {} !!! b@Rectangle {} = not (top a < bottom b || top b < bottom a || left a > right b || left b > right a)
a@Circle {} !!! b@Circle {} = let separation = centre a - centre b
                                  sq_dist = sq_mag separation
                                  sum_radii = radius a + radius b
                               in sq_dist < sum_radii * sum_radii

-- returns a Maybe of the vector needed to push the second argument so it doesn't overlap the first
infixl 2 !!>
(!!>) :: Shape -> Shape -> Maybe Vector
a@Rectangle {} !!> b@Rectangle {} = foldl1 smallest [ push move_up (top a - bottom b),
                                                      push move_down (top b - bottom a),
                                                      push move_left (right b - left a),
                                                      push move_right (right a - left b) ] where
                                           push f v = if v <= 0 then Nothing else Just $ f v
                                           smallest a b = pure min <*> a <*> b
a@Circle {} !!> b@Circle {} = let separation = centre b - centre a
                                  sq_dist = sq_mag separation
                                  required_dist = radius a + radius b
                               in if sq_dist > (required_dist * required_dist)
                                  then Nothing
                                  else let dist = sqrt sq_dist
                                           additional_dist = required_dist - dist
                                           lengths = additional_dist / dist
                                        in Just $ separation * Vector { x = lengths , y = lengths }

instance Renderable Shape where
  render r@Rectangle {} = let width = right r - left r
                              height = top r - bottom r
                              rect = rectangleSolid width height
                              shiftRight = left r + (width / 2)
                              shiftUp = bottom r + (height / 2)
                           in translate shiftRight shiftUp
                              $ rect
  render c@Circle {} = translate (x $ centre c) (y $ centre c)
                       $ circleSolid (radius c)
