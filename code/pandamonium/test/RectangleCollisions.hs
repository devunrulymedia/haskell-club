{-# OPTIONS_GHC -F -pgmF htfpp #-}
module RectangleCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Generators
import CollisionAssertions
import Graphics.Gloss.Data.Vector
import Shapes.Shape

test_overlapping_DeRectangles_collide =
  collisionBetween (DeRectangle 10 30 10 (-10)) (DeRectangle 10 30 30 5)

test_gap_above_DeRectangle_means_no_collision =
  noCollisionBetween (DeRectangle 10 30 10 (-10)) (DeRectangle 10 30 30 15)

test_gap_below_DeRectangle_means_no_collision =
  noCollisionBetween (DeRectangle 10 30 50 35) (DeRectangle 10 30 30 15)

test_gap_to_left_of_DeRectangle_means_no_collision =
  noCollisionBetween (DeRectangle 30 50 30 15) (DeRectangle 10 25 30 15)

test_gap_to_right_of_DeRectangle_means_no_collision =
  noCollisionBetween (DeRectangle 10 25 30 15) (DeRectangle 30 50 30 15)

test_no_overlap_means_no_pushout =
  assertEqual Nothing (DeRectangle 10 30 10 10 !!> DeRectangle 10 30 30 15)

test_overlapping_top_of_first_pushes_other_DeRectangle_up =
  assertEqual (Just (0, 5)) (DeRectangle 10 30 10 (-10) !!> DeRectangle 10 30 30 5)

test_overlapping_bottom_of_first_pushes_other_DeRectangle_down =
  assertEqual (Just (0, -5)) (DeRectangle 10 30 40 20 !!> DeRectangle 10 30 25 5)

test_overlapping_left_of_first_pushes_other_DeRectangle_left =
  assertEqual (Just (-2, 0)) (DeRectangle 30 50 40 20 !!> DeRectangle 10 32 40 20)

test_overlapping_right_of_first_pushes_other_DeRectangle_right =
  assertEqual (Just (3, 0)) (DeRectangle 10 30 40 20 !!> DeRectangle 27 50 40 20)

test_overlapping_a_corner_pushes_down_if_that_is_shorter_than_right =
  assertEqual (Just (0, -2)) (DeRectangle 10 30 40 20 !!> DeRectangle 27 50 22 0)

test_overlapping_a_corner_pushes_right_if_that_is_shorter_than_down =
  assertEqual (Just (3, 0)) (DeRectangle 10 30 40 20 !!> DeRectangle 27 50 24 0)

prop_colliding_shapes_have_pushout :: DeRectangle -> DeRectangle -> Bool
prop_colliding_shapes_have_pushout a b
  | a !!! b   = isJust (a !!> b)
  | otherwise = isNothing (a !!> b)

prop_collision_is_commutative :: DeRectangle -> DeRectangle -> Bool
prop_collision_is_commutative a b
  | a !!! b   = b !!! a
  | otherwise = not (b !!! a)

prop_pushout_exists_when_collision :: DeRectangle -> DeRectangle -> Bool
prop_pushout_exists_when_collision a b = case a !!> b of
  (Just _)  -> a !!! b
  (Nothing) -> not (a !!! b)

prop_pushout_is_inversely_commutative :: DeRectangle -> DeRectangle -> Bool
prop_pushout_is_inversely_commutative a b = (a !!> b) == (negate <$> (b !!> a))

-- floating point means sometimes we have a tiny pushout
prop_pushout_leaves_shapes_not_colliding :: DeRectangle -> DeRectangle -> Bool
prop_pushout_leaves_shapes_not_colliding a b = case a !!> b of
  (Just v)  -> maybe 0 magV (a !!> move v b) < (magV v) * 0.00001
  (Nothing) -> not (a !!! b)
