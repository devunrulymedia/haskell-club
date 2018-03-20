{-# OPTIONS_GHC -F -pgmF htfpp #-}
module RectangleCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Generators
import CollisionAssertions
import Graphics.Gloss.Data.Vector
import Movable
import Collisions
import Shapes.Shape
import Shapes.Movables
import Shapes.Collisions

test_overlapping_rectangles_collide =
  collisionBetween (Rectangle 10 30 10 (-10)) (Rectangle 10 30 30 5)

test_gap_above_rectangle_means_no_collision =
  noCollisionBetween (Rectangle 10 30 10 (-10)) (Rectangle 10 30 30 15)

test_gap_below_rectangle_means_no_collision =
  noCollisionBetween (Rectangle 10 30 50 35) (Rectangle 10 30 30 15)

test_gap_to_left_of_rectangle_means_no_collision =
  noCollisionBetween (Rectangle 30 50 30 15) (Rectangle 10 25 30 15)

test_gap_to_right_of_rectangle_means_no_collision =
  noCollisionBetween (Rectangle 10 25 30 15) (Rectangle 30 50 30 15)

test_no_overlap_means_no_pushout =
  assertEqual Nothing (Rectangle 10 30 10 10 !!> Rectangle 10 30 30 15)

test_overlapping_top_of_first_pushes_other_rectangle_up =
  assertEqual (Just (0, 5)) (Rectangle 10 30 10 (-10) !!> Rectangle 10 30 30 5)

test_overlapping_bottom_of_first_pushes_other_rectangle_down =
  assertEqual (Just (0, -5)) (Rectangle 10 30 40 20 !!> Rectangle 10 30 25 5)

test_overlapping_left_of_first_pushes_other_rectangle_left =
  assertEqual (Just (-2, 0)) (Rectangle 30 50 40 20 !!> Rectangle 10 32 40 20)

test_overlapping_right_of_first_pushes_other_rectangle_right =
  assertEqual (Just (3, 0)) (Rectangle 10 30 40 20 !!> Rectangle 27 50 40 20)

test_overlapping_a_corner_pushes_down_if_that_is_shorter_than_right =
  assertEqual (Just (0, -2)) (Rectangle 10 30 40 20 !!> Rectangle 27 50 22 0)

test_overlapping_a_corner_pushes_right_if_that_is_shorter_than_down =
  assertEqual (Just (3, 0)) (Rectangle 10 30 40 20 !!> Rectangle 27 50 24 0)

prop_colliding_shapes_have_pushout :: Rectangle -> Rectangle -> Bool
prop_colliding_shapes_have_pushout a b
  | a !!! b   = isJust (a !!> b)
  | otherwise = isNothing (a !!> b)

prop_collision_is_commutative :: Rectangle -> Rectangle -> Bool
prop_collision_is_commutative a b
  | a !!! b   = b !!! a
  | otherwise = not (b !!! a)

prop_pushout_exists_when_collision :: Rectangle -> Rectangle -> Bool
prop_pushout_exists_when_collision a b = case a !!> b of
  (Just _)  -> a !!! b
  (Nothing) -> not (a !!! b)

prop_pushout_is_inversely_commutative :: Rectangle -> Rectangle -> Bool
prop_pushout_is_inversely_commutative a b = (a !!> b) == (negate <$> (b !!> a))

-- floating point means sometimes we have a tiny pushout
prop_pushout_leaves_shapes_not_colliding :: Rectangle -> Rectangle -> Bool
prop_pushout_leaves_shapes_not_colliding a b = case a !!> b of
  (Just v)  -> maybe 0 magV (a !!> move b v) < (magV v) * 0.00001
  (Nothing) -> not (a !!! b)
