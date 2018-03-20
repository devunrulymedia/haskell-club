module Shapes.Collisions where

import Shapes.Datatypes
import Collisions
import Graphics.Gloss.Data.Vector

sqMagV :: Vector -> Float
sqMagV (x, y) = x * x + y * y

data RangeComparison = Before | Inside | After

compareRange :: Ord a => a -> a -> a -> RangeComparison
compareRange start end point
  | point < start = Before
  | point <= end  = Inside
  | otherwise     = After

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

rectCircInteraction :: (Vector -> Circle -> a) ->
                       (Vector -> Float -> Float -> Float -> a) ->
                       (Rectangle -> Circle -> a) ->
                       Rectangle -> Circle -> a
rectCircInteraction pointCircle projectionCircle inside rect@(Rectangle l r t b) circ@(Circle (x, y) radius) =
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

rectCircCollision = rectCircInteraction pointCircleCollision projectionCircleCollision insideCollision

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

rectCircPushout = rectCircInteraction pointCirclePushout projectionCirclePushout insidePushout

instance Collides Shape where
  a !!! b = case (deconstruct a, deconstruct b) of
    (Left x,  Left y)  -> x !!! y
    (Right x, Right y) -> x !!! y
    (Left x,  Right y) -> rectCircCollision x y
    (Right x, Left y)  -> b !!! a
  a !!> b = case (deconstruct a, deconstruct b) of
    (Left x,  Left y)  -> x !!> y
    (Right x, Right y) -> x !!> y
    (Left x,  Right y) -> rectCircPushout x y
    (Right x, Left y)  -> negate <$> (b !!> a)