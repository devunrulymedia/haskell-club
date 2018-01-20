{-# OPTIONS_GHC -F -pgmF htfpp #-}
module CircleCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Generators
import CollisionAssertions
import Graphics.Gloss.Data.Vector
import Shape

test_circles_sharing_a_centre_overlap =
  collisionBetween (Circle  (20, 30) 20) (Circle (20, 30) 10)

test_horizontally_separated_circles_touching_exactly_do_not_overlap =
  noCollisionBetween (Circle (20, 30) 60) (Circle (120, 30) 40)

test_vertically_separated_circles_touching_exactly_do_not_overlap =
  noCollisionBetween (Circle (20, 30) 60) (Circle (20, 130) 40)

test_horizontally_separated_with_sum_of_radii_just_exceeding_separation =
  collisionBetween (Circle (20, 30) 60) (Circle (120, 30) 41)

test_vertically_separated_with_sum_of_radii_just_exceeding_separation =
  collisionBetween (Circle (20, 30) 60) (Circle (20, 130) 41)

test_diagonally_touching_exactly_less_than_sum_of_radii_on_any_one_axis =
  noCollisionBetween (Circle (0, 0) 20) (Circle (30, 40) 30)

test_diagonally_overlapping_less_than_sum_of_radii_on_any_one_axis =
  collisionBetween (Circle (0, 0) 21) (Circle (30, 40) 30)

test_non_overlapping_circles_dont_push_out =
  assertEqual (Circle (0, 0) 10 !!> Circle (20, 0) 5) Nothing

test_horizontally_overlapping_circles =
  assertEqual (Circle (0, 0) 10 !!> Circle (20, 0) 15) (Just (5, 0))

test_vertically_overlapping_circles =
  assertEqual (Circle (0, 0) 10 !!> Circle (0, 20) 15) (Just (0, 5))

prop_colliding_shapes_have_pushout :: Circle -> Circle -> Bool
prop_colliding_shapes_have_pushout a b
 | a !!! b   = isJust (a !!> b)
 | otherwise = isNothing (a !!> b)

prop_collision_is_commutative :: Circle -> Circle -> Bool
prop_collision_is_commutative a b
 | a !!! b   = b !!! a
 | otherwise = not (b !!! a)

prop_pushout_exists_when_collision :: Circle -> Circle -> Bool
prop_pushout_exists_when_collision a b = case a !!> b of
 (Just _)  -> a !!! b
 (Nothing) -> not (a !!! b)

prop_pushout_is_inversely_commutative :: Circle -> Circle -> Bool
prop_pushout_is_inversely_commutative a b = (a !!> b) == (negate <$> (b !!> a))

-- floating point means sometimes we have a tiny pushout
prop_pushout_leaves_shapes_not_colliding :: Circle -> Circle -> Bool
prop_pushout_leaves_shapes_not_colliding a b = case a !!> b of
 (Just v)  -> maybe 0 magV (a !!> move b v) < (magV v) * 0.00001
 (Nothing) -> not (a !!! b)
