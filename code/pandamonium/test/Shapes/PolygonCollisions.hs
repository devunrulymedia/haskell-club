{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Shapes.PolygonCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Shapes.CollisionAssertions
import Shapes.Collisions.PolygonCollisions
import Shapes.Polygon
import Graphics.Gloss.Data.Vector
import Shapes.Shape

test_horizontally_separated_triangles_dont_collide =
  noCollisionBetween (poly [(0, 0), (3, 0), (3, 4)]) (poly [(4, 0), (4, 3), (8, 0)])

test_collides_with_itself =
  collisionBetween (poly [(0, 0), (3, 0), (0, 4)]) (poly [(0, 0), (3, 0), (0, 4)])

test_overlapping_tips_collide =
  collisionBetween (poly [(0, 0), (3, 0), (0, 4)]) (poly [(2, 0), (6, 0), (6, 3)])

test_diagonally_separated_triangles_dont_collide =
  noCollisionBetween (poly [(0, 0), (3, 0), (0, 4)]) (poly [(4, 0), (1, 4), (4, 4)])
