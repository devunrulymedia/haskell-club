module Shapes.Collisions.PolygonCollisions where

import Graphics.Gloss.Data.Vector
import Shapes.Collisions.CollisionClass
import Shapes.Polygon
import Data.Maybe
import Debug.Trace
import Shapes.Datatypes
import Shapes.Movables
import Control.Applicative

data DePoly = DePoly [ Vector ] [ Vector ] deriving (Show, Eq)

poly :: [ Vector ] -> DePoly
poly points = let (Polygon ps ns) = polygon points
               in DePoly ps ns

instance Movable DePoly where
  move v (DePoly points normals) = DePoly ((+ v) <$> points) normals

project :: [ Vector ] -> Vector -> (Float, Float)
project points normal = let projections = (dotV normal) <$> points
                         in (foldl1 min projections, foldl1 max projections)

pushProj :: (Float, Float) -> (Float, Float) -> Maybe Float
pushProj (min1, max1) (min2, max2)
  | min1 > max2 = Nothing
  | max1 < min2 = Nothing
  | otherwise   = Just $ smaller (min1 - max2) (max1 - min2)
    where smaller a b = if abs a < abs b then a else b

push :: [ Vector ] -> [ Vector ] -> Vector -> Maybe Vector
push p1s p2s normal = let proj1 = project p1s normal
                          proj2 = project p2s normal
                          pushDist = pushProj proj1 proj2
                       in (flip mulSV normal) <$> pushDist

pushOut :: DePoly -> DePoly -> Maybe Vector
pushOut (DePoly p1s n1s) (DePoly p2s n2s) = foldl1 (liftA2 shorter) $ push p1s p2s <$> (n1s ++ n2s)
  where shorter a b = if sqMagV a < sqMagV b then a else b

instance Collides DePoly where
  (!!!) a b = isJust $ pushOut a b
  (!!>) a b = pushOut a b
