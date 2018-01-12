module Shape where

import Vector
import Renderable
import Graphics.Gloss (translate, circleSolid, rectangleSolid)

data Rectangle = Rectangle Float Float Float Float deriving (Show, Eq)
data Circle = Circle Vector Float deriving (Show, Eq)

data Shape = Rect Rectangle | Circ Circle deriving (Show, Eq)

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
infixl 2 !!>

class Collides a where
  (!!!) :: a -> a -> Bool
  (!!>) :: a -> a -> Maybe Vector

instance Collides Rectangle where
  (Rectangle la ra ta ba) !!! (Rectangle lb rb tb bb) = not (ta <= bb || tb <= ba || la >= rb || lb >= ra)
  (Rectangle la ra ta ba) !!> (Rectangle lb rb tb bb) = foldl1 smallest [ push move_up (ta - bb),
                                                                          push move_down (tb - ba),
                                                                          push move_left (rb - la),
                                                                          push move_right (ra - lb) ] where
                                                               push f v = if v <= 0 then Nothing else Just $ f v
                                                               smallest a b = pure min <*> a <*> b

instance Collides Circle where
  (Circle ca ra) !!! (Circle cb rb) = sq_mag (cb - ca) < (ra + rb) * (ra + rb)
  (Circle ca ra) !!> (Circle cb rb) = pushout where
     separation = cb - ca
     sq_dist = sq_mag separation
     required_dist = ra + rb
     pushout
       | sq_dist > (required_dist * required_dist) = Nothing
       | sq_dist == 0                              = Just $ Vector { x = 0, y = if ra > rb
                                                                              then required_dist
                                                                              else -required_dist }
       | otherwise                                 = let dist = sqrt sq_dist
                                                         additional_dist = required_dist - dist
                                                         lengths = additional_dist / dist
                                                      in Just $ separation * Vector { x = lengths , y = lengths }

instance Collides Shape where
  (Rect a) !!! (Rect b) = a !!! b
  (Circ a) !!! (Circ b) = a !!! b
  (Rect a) !!> (Rect b) = a !!> b
  (Circ a) !!> (Circ b) = a !!> b

instance Renderable Rectangle where
  render (Rectangle l r t b) = translate (l + width / 2) (b + height / 2)
                             $ rectangleSolid width height where
                             width = r - l
                             height = t - b

instance Renderable Circle where
  render (Circle c r) = translate (x c) (y c) $ circleSolid r

instance Renderable Shape where
  render (Rect r) = render r
  render (Circ c) = render c
