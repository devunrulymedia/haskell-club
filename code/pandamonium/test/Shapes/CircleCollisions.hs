{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Shapes.CircleCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Shapes.Generators
import Shapes.CollisionAssertions
import Graphics.Gloss.Data.Vector
import Thrust.Shapes.Shape

test_circles_sharing_a_centre_overlap =
  collisionBetween (circle  (20, 30) 20) (circle (20, 30) 10)

test_horizontally_separated_circles_touching_exactly_do_not_overlap =
  noCollisionBetween (circle (20, 30) 60) (circle (120, 30) 40)

test_vertically_separated_circles_touching_exactly_do_not_overlap =
  noCollisionBetween (circle (20, 30) 60) (circle (20, 130) 40)

test_horizontally_separated_with_sum_of_radii_just_exceeding_separation =
  collisionBetween (circle (20, 30) 60) (circle (120, 30) 41)

test_vertically_separated_with_sum_of_radii_just_exceeding_separation =
  collisionBetween (circle (20, 30) 60) (circle (20, 130) 41)

test_diagonally_nearly_touching_less_than_sum_of_radii_on_any_one_axis =
  noCollisionBetween (circle (0, 0) 20) (circle (30, 40) 29.8)

test_diagonally_overlapping_less_than_sum_of_radii_on_any_one_axis =
  collisionBetween (circle (0, 0) 21) (circle (30, 40) 30)

test_non_overlapping_circles_dont_push_out =
  assertEqual (circle (0, 0) 10 !!> circle (20, 0) 5) Nothing

test_horizontally_overlapping_circles =
  assertEqual (circle (0, 0) 10 !!> circle (20, 0) 15) (Just (5, 0))

test_vertically_overlapping_circles =
  assertEqual (circle (0, 0) 10 !!> circle (0, 20) 15) (Just (0, 5))
