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
  (Rectangle la ra ta ba) !!> (Rectangle lb rb tb bb) = foldl1 smallest [ push move_up (bb - ta),
                                                                          push move_down (ba - tb),
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
                                                      in Just $ separation * scale lengths

instance Collides Shape where
  (Rect a) !!! (Rect b) = a !!! b
  (Circ a) !!! (Circ b) = a !!! b
  (Rect a) !!! (Circ b) = rectCircCollision pointCircleCollision projectionCircleCollision insideCollision a b
  a@(Circ _) !!! b@(Rect _) = b !!! a
  (Rect a) !!> (Rect b) = a !!> b
  (Circ a) !!> (Circ b) = a !!> b
  (Rect a) !!> (Circ b) = rectCircCollision pointCirclePushout projectionCirclePushout insidePushout a b
  a@(Circ _) !!> b@(Rect _) = negate <$> (b !!> a)

data RangeComparison = Before | Inside | After

compareRange :: Ord a => a -> a -> a -> RangeComparison
compareRange start end point
  | point < start = Before
  | point <= end  = Inside
  | otherwise     = After

rectCircCollision :: (Vector -> Circle -> a) ->
                     (Vector -> Float -> Float -> Float -> a) ->
                     (Rectangle -> Circle -> a) ->
                     Rectangle -> Circle -> a
rectCircCollision pointCircle projectionCircle inside rect@(Rectangle l r t b) circ@(Circle centre radius) =
   case (compareRange l r (x centre), compareRange b t (y centre)) of
     (Before, Before) -> pointCircle Vector { x = l, y = b } circ
     (Before, After)  -> pointCircle Vector { x = l, y = t } circ
     (After, Before)  -> pointCircle Vector { x = r, y = b } circ
     (After, After)   -> pointCircle Vector { x = r, y = t } circ
     (Before, Inside) -> projectionCircle (move_left 1)  l (x centre) radius
     (After, Inside)  -> projectionCircle (move_right 1) r (x centre) radius
     (Inside, Before) -> projectionCircle (move_down 1)  b (y centre) radius
     (Inside, After)  -> projectionCircle (move_up 1)    t (y centre) radius
     (Inside, Inside) -> inside rect circ

pointCircleCollision :: Vector -> Circle -> Bool
pointCircleCollision corner circle = (Circle corner 0) !!! circle

projectionCircleCollision :: Vector -> Float -> Float -> Float -> Bool
projectionCircleCollision _ lineProj circleProj radius = radius > (abs $ lineProj - circleProj)

insideCollision :: Rectangle -> Circle -> Bool
insideCollision _ _ = True

pointCirclePushout :: Vector -> Circle -> Maybe Vector
pointCirclePushout corner circle = (Circle corner 0) !!> circle

projectionCirclePushout :: Vector -> Float -> Float -> Float -> Maybe Vector
projectionCirclePushout unit lineProj circleProj radius = if radius > sep
      then Just $ unit * scale req
      else Nothing
      where sep = abs $ lineProj - circleProj
            req = radius - sep

insidePushout :: Rectangle -> Circle -> Maybe Vector
insidePushout (Rectangle left right top bottom) (Circle centre radius) =
  Just $ minimum [ move_up (radius + top - y centre),
                   move_down (radius + y centre - bottom),
                   move_left (radius + x centre - left),
                   move_right (radius + right - x centre) ]


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
