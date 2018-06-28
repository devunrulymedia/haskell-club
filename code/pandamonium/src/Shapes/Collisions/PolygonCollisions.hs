module Shapes.Collisions.PolygonCollisions where

import Graphics.Gloss.Data.Vector
import Shapes.Collisions.CollisionClass
import Shapes.Polygon
import Data.Maybe
import Debug.Trace
import Shapes.Datatypes
import Shapes.Movables

data DePoly = DePoly [ Vector ] [ Vector ] deriving (Show, Eq)

poly :: [ Vector ] -> DePoly
poly points = let (Polygon ps ns) = polygon points
               in DePoly ps ns

instance Movable DePoly where
  move v (DePoly points normals) = DePoly ((+ v) <$> points) normals

project :: [ Vector ] -> Vector -> (Float, Float)
project points normal = let projections = (dotV normal) <$> points
                         in (foldl1 min projections, foldl1 max projections)

push :: (Float, Float) -> (Float, Float) -> Maybe Float
push (min1, max1) (min2, max2)
  | min1 > max2 = Nothing
  | max1 < min2 = Nothing
  | otherwise   = let pushLeft = min1 - max2
                      pushRight = max1 - min2
                   in if abs pushLeft < abs pushRight
                 then Just pushLeft
                 else Just pushRight

pushes :: DePoly -> DePoly -> [ (Vector, Maybe Float) ]
pushes (DePoly p1s n1s) (DePoly p2s n2s) = push' p1s p2s <$> (n1s ++ n2s) where
  push' p1s p2s normal = let proj1 = project p1s normal
                             proj2 = project p2s normal
                             pushout = push proj1 proj2
                          in (normal, pushout)

shorter :: Vector -> Vector -> Vector
shorter a b = if sqMagV a < sqMagV b then a else b

pushOut :: DePoly -> DePoly -> Maybe Vector
pushOut (DePoly p1s n1s) (DePoly p2s n2s) = pushOut' (n1s ++ n2s) Nothing where
  pushOut' :: [ Vector ] -> Maybe Vector -> Maybe Vector
  pushOut' [] shortest = shortest
  pushOut' (normal : rest) shortest =
    let proj1 = project p1s normal
        proj2 = project p2s normal
     in case (flip mulSV normal) <$> push proj1 proj2 of
        Nothing        -> Nothing
        (Just current) -> pushOut' rest $ case shortest of
          Nothing     -> Just current
          (Just prev) -> Just $ shorter prev current

instance Collides DePoly where
  (!!!) a b = isJust $ pushOut a b
  (!!>) a b = pushOut a b
