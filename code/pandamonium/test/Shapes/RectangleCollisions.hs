{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Shapes.RectangleCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Shapes.Generators
import Shapes.CollisionAssertions
import Graphics.Gloss.Data.Vector
import Shapes.Shape

test_overlapping_rectangles_collide =
  collisionBetween (rectangle 10 30 10 (-10)) (rectangle 10 30 30 5)

test_gap_above_rectangle_means_no_collision =
  noCollisionBetween (rectangle 10 30 10 (-10)) (rectangle 10 30 30 15)

test_gap_below_rectangle_means_no_collision =
  noCollisionBetween (rectangle 10 30 50 35) (rectangle 10 30 30 15)

test_gap_to_left_of_rectangle_means_no_collision =
  noCollisionBetween (rectangle 30 50 30 15) (rectangle 10 25 30 15)

test_gap_to_right_of_rectangle_means_no_collision =
  noCollisionBetween (rectangle 10 25 30 15) (rectangle 30 50 30 15)

test_no_overlap_means_no_pushout =
  assertEqual Nothing (rectangle 10 30 10 10 !!> rectangle 10 30 30 15)

test_overlapping_top_of_first_pushes_other_rectangle_up =
  assertEqual (Just (0, 5)) (rectangle 10 30 10 (-10) !!> rectangle 10 30 30 5)

test_overlapping_bottom_of_first_pushes_other_rectangle_down =
  assertEqual (Just (0, -5)) (rectangle 10 30 40 20 !!> rectangle 10 30 25 5)

test_overlapping_left_of_first_pushes_other_rectangle_left =
  assertEqual (Just (-2, 0)) (rectangle 30 50 40 20 !!> rectangle 10 32 40 20)

test_overlapping_right_of_first_pushes_other_rectangle_right =
  assertEqual (Just (3, 0)) (rectangle 10 30 40 20 !!> rectangle 27 50 40 20)

test_overlapping_a_corner_pushes_down_if_that_is_shorter_than_right =
  assertEqual (Just (0, -2)) (rectangle 10 30 40 20 !!> rectangle 27 50 22 0)

test_overlapping_a_corner_pushes_right_if_that_is_shorter_than_down =
  assertEqual (Just (3, 0)) (rectangle 10 30 40 20 !!> rectangle 27 50 24 0)

-- prop_colliding_shapes_have_pushout :: rectangle -> rectangle -> Bool
-- prop_colliding_shapes_have_pushout a b
--   | a !!! b   = isJust (a !!> b)
--   | otherwise = isNothing (a !!> b)
--
-- prop_collision_is_commutative :: rectangle -> rectangle -> Bool
-- prop_collision_is_commutative a b
--   | a !!! b   = b !!! a
--   | otherwise = not (b !!! a)
--
-- prop_pushout_exists_when_collision :: rectangle -> rectangle -> Bool
-- prop_pushout_exists_when_collision a b = case a !!> b of
--   (Just _)  -> a !!! b
--   (Nothing) -> not (a !!! b)
--
-- prop_pushout_is_inversely_commutative :: rectangle -> rectangle -> Bool
-- prop_pushout_is_inversely_commutative a b = (a !!> b) == (negate <$> (b !!> a))
--
-- -- floating point means sometimes we have a tiny pushout
-- prop_pushout_leaves_shapes_not_colliding :: rectangle -> rectangle -> Bool
-- prop_pushout_leaves_shapes_not_colliding a b = case a !!> b of
--   (Just v)  -> maybe 0 magV (a !!> move v b) < (magV v) * 0.00001
--   (Nothing) -> not (a !!! b)
