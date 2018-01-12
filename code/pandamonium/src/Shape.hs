module Shape where

import Vector
import Renderable
import Graphics.Gloss (translate, circleSolid, rectangleSolid)

data Rectangle = Rectangle Float Float Float Float
data Circle = Circle Vector Float

data Shape = Rect Rectangle | Circ Circle

class Movable a where
  move :: a -> Vector -> a

instance Movable Rectangle where
  move (Rectangle l r t b) v = Rectangle (l + x v) (r + x v) (t + y v) (b + y v)

instance Movable Circle where
  move (Circle c r) v = Circle (c + v) r

instance Movable Shape where
  move (Rect r) v = Rect (move r v)
  move (Circ c) v = Circ (move c v)

infixl 2 !!!
class Collides a where
  (!!!) :: a -> a -> Bool
  (!!>) :: a -> a -> Maybe Vector

-- checks if two shapes collide, returning a boolean
infixl 2 !!!
infixl 2 !!>

instance Collides Rectangle where
  (Rectangle la ra ta ba) !!! (Rectangle lb rb tb bb) = not (ta <= bb || tb <= ba || la >= rb || lb >= ra)
  (Rectangle la ra ta ba) !!! (Rectangle lb rb tb bb) = foldl1 smallest [ push move_up (top a - bottom b),
                              push move_down (top b - bottom a),
                              push move_left (right b - left a),
                              push move_right (right a - left b) ] where
                   push f v = if v <= 0 then Nothing else Just $ f v
                   smallest a b = pure min <*> a <*> b

instance Collides Circle where
  a !!! b = let separation = centre a - centre b
                sq_dist = sq_mag separation
                sum_radii = radius a + radius b
             in sq_dist < sum_radii * sum_radii
  a !!> b = pushout where
     separation = centre b - centre a
     sq_dist = sq_mag separation
     required_dist = radius a + radius b
     pushout
       | sq_dist > (required_dist * required_dist) = Nothing
       | sq_dist == 0                              = Just $ Vector { x = 0, y = if radius a > radius b
                                                                              then required_dist
                                                                              else -required_dist }
       | otherwise                                 = let dist = sqrt sq_dist
                                                         additional_dist = required_dist - dist
                                                         lengths = additional_dist / dist
                                                      in Just $ separation * Vector { x = lengths , y = lengths }
(!!!) :: Shape -> Shape -> Bool
a@Rectangle {} !!! b@Rectangle {} = not (top a <= bottom b || top b <= bottom a || left a >= right b || left b >= right a)
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
a@Circle {} !!> b@Circle {} = pushout where
   separation = centre b - centre a
   sq_dist = sq_mag separation
   required_dist = radius a + radius b
   pushout
     | sq_dist > (required_dist * required_dist) = Nothing
     | sq_dist == 0                              = Just $ Vector { x = 0, y = if radius a > radius b
                                                                            then required_dist
                                                                            else -required_dist }
     | otherwise                                 = let dist = sqrt sq_dist
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
