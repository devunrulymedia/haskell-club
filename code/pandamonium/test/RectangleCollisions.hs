{-# OPTIONS_GHC -F -pgmF htfpp #-}
module RectangleCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Generators
import Vector
import Shape

test_overlapping_rectangles_collide =
  let rectA = Rectangle { left = 10, right = 30, top = 10, bottom = -10 }
      rectB = Rectangle { left = 10, right = 30, top = 30, bottom = 5 }
   in collisionBetween rectA rectB

test_gap_above_rectangle_means_no_collision =
  let rectA = Rectangle { left = 10, right = 30, top = 10, bottom = -10 }
      rectB = Rectangle { left = 10, right = 30, top = 30, bottom = 15 }
   in noCollisionBetween rectA rectB

test_gap_below_rectangle_means_no_collision =
  let rectA = Rectangle { left = 10, right = 30, top = 50, bottom = 35 }
      rectB = Rectangle { left = 10, right = 30, top = 30, bottom = 15 }
   in noCollisionBetween rectA rectB

test_gap_to_left_of_rectangle_means_no_collision =
  let rectA = Rectangle { left = 30, right = 50, top = 30, bottom = 15 }
      rectB = Rectangle { left = 10, right = 25, top = 30, bottom = 15 }
   in noCollisionBetween rectA rectB

test_gap_to_right_of_rectangle_means_no_collision =
  let rectA = Rectangle { left = 10, right = 25, top = 30, bottom = 15 }
      rectB = Rectangle { left = 30, right = 50, top = 30, bottom = 15 }
   in noCollisionBetween rectA rectB

test_no_overlap_means_no_pushout =
  let rectA = Rectangle { left = 10, right = 30, top = 10, bottom = -10 }
      rectB = Rectangle { left = 10, right = 30, top = 30, bottom = 15 }
   in assertEqual (rectA !!> rectB) Nothing

test_overlapping_top_of_first_pushes_other_rectangle_up =
  let rectA = Rectangle { left = 10, right = 30, top = 10, bottom = -10 }
      rectB = Rectangle { left = 10, right = 30, top = 30, bottom = 5 }
   in assertEqual (rectA !!> rectB) (Just (move_up 5))

test_overlapping_bottom_of_first_pushes_other_rectangle_down =
  let rectA = Rectangle { left = 10, right = 30, top = 40, bottom = 20 }
      rectB = Rectangle { left = 10, right = 30, top = 25, bottom = 5 }
   in assertEqual (rectA !!> rectB) (Just (move_down 5))

test_overlapping_left_of_first_pushes_other_rectangle_left =
   let rectA = Rectangle { left = 30, right = 50, top = 40, bottom = 20 }
       rectB = Rectangle { left = 10, right = 32, top = 40, bottom = 20 }
    in assertEqual (rectA !!> rectB) (Just (move_left 2))

test_overlapping_right_of_first_pushes_other_rectangle_right =
  let rectA = Rectangle { left = 10, right = 30, top = 40, bottom = 20 }
      rectB = Rectangle { left = 27, right = 50, top = 40, bottom = 20 }
   in assertEqual (rectA !!> rectB) (Just (move_right 3))

test_overlapping_a_corner_pushes_down_if_that_is_shorter_than_right =
  let rectA = Rectangle { left = 10, right = 30, top = 40, bottom = 20 }
      rectB = Rectangle { left = 27, right = 50, top = 22, bottom = 0 }
   in assertEqual (rectA !!> rectB) (Just (move_down 2))

test_overlapping_a_corner_pushes_right_if_that_is_shorter_than_down =
  let rectA = Rectangle { left = 10, right = 30, top = 40, bottom = 20 }
      rectB = Rectangle { left = 27, right = 50, top = 24, bottom = 0 }
   in assertEqual (rectA !!> rectB) (Just (move_right 3))

prop_colliding_rectangles_have_pushout :: Shape -> Shape -> Bool
prop_colliding_rectangles_have_pushout a@Rectangle{} b@Rectangle{}
  | a !!! b = isJust (a !!> b)
  | otherwise  = isNothing (a !!> b)
prop_colliding_rectangles_have_pushout _ _ = True

prop_pushout_makes_rectangles_no_longer_collide :: Shape -> Shape -> Bool
prop_pushout_makes_rectangles_no_longer_collide a@Rectangle{} b@Rectangle{} =
  case a !!> b of
    (Just v)  -> not (a !!! move b v)
    (Nothing) -> not (a !!! b)
prop_pushout_makes_rectangles_no_longer_collide _ _ = True

prop_collisions_are_commutative :: Shape -> Shape -> Bool
prop_collisions_are_commutative a@Rectangle{} b@Rectangle{}
  | a !!! b = b !!! a
  | otherwise  = not (b !!! a)
prop_collisions_are_commutative _ _ = True

prop_pushouts_are_inversely_commutative :: Shape -> Shape -> Bool
prop_pushouts_are_inversely_commutative a@Rectangle{} b@Rectangle{} = (a !!> b) == (negate <$> (b !!> a))
prop_pushouts_are_inversely_commutative _ _ = True


collisionBetween a b = unless (a !!! b) (assertFailure msg)
 where msg = "\n* expected: " ++ show a ++ " to collide with " ++ show b ++ "\n* but got: no collision"

noCollisionBetween a b = when (a !!! b) (assertFailure msg)
 where msg = "\n* expected: " ++ show a ++ " to not collide with " ++ show b ++ "\n* but got: a collision"
