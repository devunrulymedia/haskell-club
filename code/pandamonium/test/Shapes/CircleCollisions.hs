{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Shapes.CircleCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Shapes.Generators
import Shapes.CollisionAssertions
import Graphics.Gloss.Data.Vector
import Shapes.Shape

test_DeCircles_sharing_a_centre_overlap =
  collisionBetween (DeCircle  (20, 30) 20) (DeCircle (20, 30) 10)

test_horizontally_separated_DeCircles_touching_exactly_do_not_overlap =
  noCollisionBetween (DeCircle (20, 30) 60) (DeCircle (120, 30) 40)

test_vertically_separated_DeCircles_touching_exactly_do_not_overlap =
  noCollisionBetween (DeCircle (20, 30) 60) (DeCircle (20, 130) 40)

test_horizontally_separated_with_sum_of_radii_just_exceeding_separation =
  collisionBetween (DeCircle (20, 30) 60) (DeCircle (120, 30) 41)

test_vertically_separated_with_sum_of_radii_just_exceeding_separation =
  collisionBetween (DeCircle (20, 30) 60) (DeCircle (20, 130) 41)

test_diagonally_touching_exactly_less_than_sum_of_radii_on_any_one_axis =
  noCollisionBetween (DeCircle (0, 0) 20) (DeCircle (30, 40) 30)

test_diagonally_overlapping_less_than_sum_of_radii_on_any_one_axis =
  collisionBetween (DeCircle (0, 0) 21) (DeCircle (30, 40) 30)

test_non_overlapping_DeCircles_dont_push_out =
  assertEqual (DeCircle (0, 0) 10 !!> DeCircle (20, 0) 5) Nothing

test_horizontally_overlapping_DeCircles =
  assertEqual (DeCircle (0, 0) 10 !!> DeCircle (20, 0) 15) (Just (5, 0))

test_vertically_overlapping_DeCircles =
  assertEqual (DeCircle (0, 0) 10 !!> DeCircle (0, 20) 15) (Just (0, 5))

prop_colliding_shapes_have_pushout :: DeCircle -> DeCircle -> Bool
prop_colliding_shapes_have_pushout a b
 | a !!! b   = isJust (a !!> b)
 | otherwise = isNothing (a !!> b)

prop_collision_is_commutative :: DeCircle -> DeCircle -> Bool
prop_collision_is_commutative a b
 | a !!! b   = b !!! a
 | otherwise = not (b !!! a)

prop_pushout_exists_when_collision :: DeCircle -> DeCircle -> Bool
prop_pushout_exists_when_collision a b = case a !!> b of
 (Just _)  -> a !!! b
 (Nothing) -> not (a !!! b)

prop_pushout_is_inversely_commutative :: DeCircle -> DeCircle -> Bool
prop_pushout_is_inversely_commutative a b = (a !!> b) == (negate <$> (b !!> a))

-- floating point means sometimes we have a tiny pushout
prop_pushout_leaves_shapes_not_colliding :: DeCircle -> DeCircle -> Bool
prop_pushout_leaves_shapes_not_colliding a b = case a !!> b of
 (Just v)  -> maybe 0 magV (a !!> move v b) < (magV v) * 0.00001
 (Nothing) -> not (a !!! b)
