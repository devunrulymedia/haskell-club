module Shapes.Collisions where

import Shapes.Datatypes
import Collisions
import Movable
import Graphics.Gloss.Data.Vector

-- because the functions here get long, and we have different implementations
-- for rect/rect, circle/circle and rect/circle cases, it's handy to put these
-- in different functions, each dealing with a separate case. To do so, we
-- create a collection of types, which together are isomorphic to our sum type,
-- and provide functions to convert between them. This allows us to have smaller
-- signatures and not co-locate all the various different patterns.

data DeRectangle = DeRectangle Float Float Float Float deriving (Show, Eq)
data DeCircle = DeCircle Vector Float deriving (Show, Eq)

deconstruct :: Shape -> Either DeRectangle DeCircle
deconstruct (Rectangle l r t b) = Left $ DeRectangle l r t b
deconstruct (Circle c r) = Right $ DeCircle c r

construct :: Either DeRectangle DeCircle -> Shape
construct (Left (DeRectangle l r t b)) = Rectangle l r t b
construct (Right (DeCircle c r)) = Circle c r

instance Movable DeRectangle where
  move (DeRectangle l r t b) (x, y ) = DeRectangle (l + x) (r + x) (t + y) (b + y)

instance Movable DeCircle where
  move (DeCircle c r) v = DeCircle (c + v) r


sqMagV :: Vector -> Float
sqMagV (x, y) = x * x + y * y

data RangeComparison = Before | Inside | After

compareRange :: Ord a => a -> a -> a -> RangeComparison
compareRange start end point
  | point < start = Before
  | point <= end  = Inside
  | otherwise     = After

instance Collides DeRectangle where
  (DeRectangle la ra ta ba) !!! (DeRectangle lb rb tb bb) = not (ta <= bb || tb <= ba || la >= rb || lb >= ra)
  (DeRectangle la ra ta ba) !!> (DeRectangle lb rb tb bb) = foldl1 smallest [ push (0, 1)  (ta - bb),
                                                                              push (0, -1) (tb - ba),
                                                                              push (-1, 0) (rb - la),
                                                                              push (1, 0)  (ra - lb) ] where
                                                               push v m = if m <= 0 then Nothing else Just $ mulSV m v
                                                               shortest a b = if sqMagV a < sqMagV b then a else b
                                                               smallest a b = pure shortest <*> a <*> b

instance Collides DeCircle where
  (DeCircle ca ra) !!! (DeCircle cb rb) = sqMagV (cb - ca) < (ra + rb) * (ra + rb)
  (DeCircle ca ra) !!> (DeCircle cb rb) = pushout where
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

rectCircInteraction :: (Vector -> DeCircle -> a) ->
                       (Vector -> Float -> Float -> Float -> a) ->
                       (DeRectangle -> DeCircle -> a) ->
                       DeRectangle -> DeCircle -> a
rectCircInteraction pointCircle projectionCircle inside rect@(DeRectangle l r t b) circ@(DeCircle (x, y) radius) =
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

pointCircleCollision :: Vector -> DeCircle -> Bool
pointCircleCollision corner circle = (DeCircle corner 0) !!! circle

projectionCircleCollision :: Vector -> Float -> Float -> Float -> Bool
projectionCircleCollision _ lineProj circleProj radius = radius > (abs $ lineProj - circleProj)

insideCollision :: DeRectangle -> DeCircle -> Bool
insideCollision _ _ = True

rectCircCollision = rectCircInteraction pointCircleCollision projectionCircleCollision insideCollision

pointCirclePushout :: Vector -> DeCircle -> Maybe Vector
pointCirclePushout corner circle = (DeCircle corner 0) !!> circle

projectionCirclePushout :: Vector -> Float -> Float -> Float -> Maybe Vector
projectionCirclePushout unit lineProj circleProj radius = if radius > sep
      then Just $ mulSV req unit
      else Nothing
      where sep = abs $ lineProj - circleProj
            req = radius - sep

insidePushout :: DeRectangle -> DeCircle -> Maybe Vector
insidePushout (DeRectangle left right top bottom) (DeCircle (x, y) radius) =
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
