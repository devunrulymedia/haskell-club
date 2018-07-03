{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Shapes.PolygonCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Shapes.Generators
import Shapes.CollisionAssertions
import Graphics.Gloss.Data.Vector
import Shapes.Shape

test_diagonally_separared_triangles_dont_collide =
  assertEqual Nothing $
  polygon [(0,0), (20, 0), (0, 20)] !!> polygon [(0, 21), (20, 21), (20, 1)]

test_diagonally_separated_triangle_and_circle_dont_collide =
  assertEqual Nothing $
  polygon [(0,0), (20, 0), (0, 20)] !!> circle (12, 12) 1
