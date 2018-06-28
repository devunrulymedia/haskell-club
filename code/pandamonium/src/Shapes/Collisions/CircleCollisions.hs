module Shapes.Collisions.CircleCollisions where

import Graphics.Gloss.Data.Vector
import Shapes.Collisions.CollisionClass
import Shapes.Movables

data DeCircle = DeCircle Vector Float deriving (Show, Eq)

instance Movable DeCircle where
  move v (DeCircle c r) = DeCircle (c + v) r

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
