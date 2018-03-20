module Collisions.Shape where

import Renderable
import Graphics.Gloss (translate, rectangleSolid, circleSolid, Vector)
import Graphics.Gloss.Data.Vector

data Rectangle = Rectangle Float Float Float Float deriving (Show, Eq)
data Circle = Circle Vector Float deriving (Show, Eq)

data Shape = Rect Rectangle | Circ Circle deriving (Show, Eq)

class Movable a where
  move :: a -> Vector -> a

instance Movable Rectangle where
  move (Rectangle l r t b) (x, y) = Rectangle (l + x) (r + x) (t + y) (b + y)

instance Movable Circle where
  move (Circle c r) v = Circle (c + v) r

instance Movable Shape where
  move (Rect r) v = Rect (move r v)
  move (Circ c) v = Circ (move c v)

class Shaped t where
  shape :: t -> Shape

infixl 2 !!!
infixl 2 !!>

class Collides a where
  (!!!) :: a -> a -> Bool
  (!!>) :: a -> a -> Maybe Vector

instance Collides Rectangle where
  (Rectangle la ra ta ba) !!! (Rectangle lb rb tb bb) = not (ta <= bb || tb <= ba || la >= rb || lb >= ra)
  (Rectangle la ra ta ba) !!> (Rectangle lb rb tb bb) = foldl1 smallest [ push (0, 1)  (ta - bb),
                                                                          push (0, -1) (tb - ba),
                                                                          push (-1, 0) (rb - la),
                                                                          push (1, 0)  (ra - lb) ] where
                                                               push v m = if m <= 0 then Nothing else Just $ mulSV m v
                                                               shortest a b = if sqMagV a < sqMagV b then a else b
                                                               smallest a b = pure shortest <*> a <*> b

instance Collides Circle where
  (Circle ca ra) !!! (Circle cb rb) = sqMagV (cb - ca) < (ra + rb) * (ra + rb)
  (Circle ca ra) !!> (Circle cb rb) = pushout where
     separation = cb - ca
     sq_dist = sqMagV separation
     required_dist = ra + rb
     pushout
       | sq_dist > (required_dist * required_dist) = Nothing
       | sq_dist == 0                              = Just $ (0, if ra > rb then required_dist else -required_dist)
       | otherwise                                 = let dist = sqrt sq_dist
                                                         additional_dist = required_dist - dist
                                                         lengths = additional_dist / dist
                                                      in Just $ mulSV lengths separation

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
rectCircCollision pointCircle projectionCircle inside rect@(Rectangle l r t b) circ@(Circle (x, y) radius) =
   case (compareRange l r x, compareRange b t y) of
     (Before, Before) -> pointCircle (l, b) circ
     (Before, After)  -> pointCircle (l, t) circ
     (After, Before)  -> pointCircle (r, b) circ
     (After, After)   -> pointCircle (r, t) circ
     (Before, Inside) -> projectionCircle (-1, 0) l x radius
     (After, Inside)  -> projectionCircle (1, 0)  r x radius
     (Inside, Before) -> projectionCircle (0, -1) b y radius
     (Inside, After)  -> projectionCircle (0, 1)  t y radius
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
      then Just $ mulSV req unit
      else Nothing
      where sep = abs $ lineProj - circleProj
            req = radius - sep

insidePushout :: Rectangle -> Circle -> Maybe Vector
insidePushout (Rectangle left right top bottom) (Circle (x, y) radius) =
  Just $ minimum [ (0, radius + top - y),
                   (0, bottom - radius - y),
                   (left - radius - x, 0),
                   (radius + right - x, 0) ]

sqMagV :: Vector -> Float
sqMagV (x, y) = x * x + y * y


instance Renderable Rectangle where
  render (Rectangle l r t b) = translate (l + width / 2) (b + height / 2)
                             $ rectangleSolid width height where
                             width = r - l
                             height = t - b

instance Renderable Circle where
  render (Circle (x, y) r) = translate x y $ circleSolid r

instance Renderable Shape where
  render (Rect r) = render r
  render (Circ c) = render c
