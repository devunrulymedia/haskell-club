module Shapes.Collisions.Collisions where

import Shapes.Datatypes
import Shapes.Movables
import Shapes.Collisions.CollisionClass
import Shapes.Collisions.CircleCollisions
import Shapes.Collisions.RectangleCollisions
import Shapes.Collisions.PolygonCollisions
import Graphics.Gloss.Data.Vector

data Deconstructed = Rec DeRectangle | Circ DeCircle | Poly DePoly

deconstruct :: Shape -> Deconstructed
deconstruct (Rectangle l r t b) = Rec $ DeRectangle l r t b
deconstruct (Circle c r) = Circ $ DeCircle c r
deconstruct (Polygon points normals) = Poly $ DePoly points normals

construct :: Deconstructed -> Shape
construct (Rec (DeRectangle l r t b)) = Rectangle l r t b
construct (Circ (DeCircle c r)) = Circle c r
construct (Poly (DePoly points normals)) = Polygon points normals

data RangeComparison = Before | Inside | After

compareRange :: Ord a => a -> a -> a -> RangeComparison
compareRange start end point
  | point < start = Before
  | point <= end  = Inside
  | otherwise     = After

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
    (Rec x,  Rec y)  -> x !!! y
    (Circ x, Circ y) -> x !!! y
    (Rec x,  Circ y) -> rectCircCollision x y
    (Circ x, Rec y)  -> b !!! a
    otherwise        -> False
  a !!> b = case (deconstruct a, deconstruct b) of
    (Rec x,  Rec y)  -> x !!> y
    (Circ x, Circ y) -> x !!> y
    (Rec x,  Circ y) -> rectCircPushout x y
    (Circ x, Rec y)  -> negate <$> (b !!> a)
    otherwise        -> Nothing
