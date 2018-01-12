{-# OPTIONS_GHC -F -pgmF htfpp #-}
module CircleCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Generators
import CollisionAssertions
import Vector
import Shape

test_circles_sharing_a_centre_overlap =
  collisionBetween (Circle Vector { x = 20, y = 30} 20) (Circle Vector { x = 20, y = 30} 10)

test_horizontally_separated_circles_touching_exactly_do_not_overlap =
  noCollisionBetween (Circle Vector { x = 20, y = 30} 60) (Circle Vector { x = 120, y = 30} 40)

test_vertically_separated_circles_touching_exactly_do_not_overlap =
  noCollisionBetween (Circle Vector { x = 20, y = 30} 60) (Circle Vector { x = 20, y = 130} 40)

test_horizontally_separated_with_sum_of_radii_just_exceeding_separation =
  collisionBetween (Circle Vector { x = 20, y = 30} 60) (Circle Vector { x = 120, y = 30} 41)

test_vertically_separated_with_sum_of_radii_just_exceeding_separation =
  collisionBetween (Circle Vector { x = 20, y = 30} 60) (Circle Vector { x = 20, y = 130} 41)

test_diagonally_touching_exactly_less_than_sum_of_radii_on_any_one_axis =
  noCollisionBetween (Circle Vector { x = 0, y = 0} 20) (Circle Vector { x = 30, y = 40} 30)

test_diagonally_overlapping_less_than_sum_of_radii_on_any_one_axis =
  collisionBetween (Circle Vector { x = 0, y = 0} 21) (Circle Vector { x = 30, y = 40} 30)

test_non_overlapping_circles_dont_push_out =
  assertEqual (Circle Vector {x = 0, y = 0} 10 !!> Circle Vector {x = 20, y = 0} 5) Nothing

test_horizontally_overlapping_circles =
  assertEqual (Circle Vector {x = 0, y = 0} 10 !!> Circle Vector {x = 20, y = 0} 15) (Just (move_right 5))

test_vertically_overlapping_circles =
  assertEqual (Circle Vector {x = 0, y = 0} 10 !!> Circle Vector {x = 0, y = 20} 15) (Just (move_up 5))

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
 (Just v)  -> maybe 0 magnitude (a !!> move b v) < (magnitude v) * 0.00001
 (Nothing) -> not (a !!! b)
