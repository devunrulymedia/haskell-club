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
  let circA = Circle { centre = Vector { x = 20, y = 30}, radius = 20 }
      circB = Circle { centre = Vector { x = 20, y = 30}, radius = 10 }
   in collisionBetween circA circB

test_horizontally_separated_circles_touching_exactly_do_not_overlap =
  let circA = Circle { centre = Vector { x = 20, y = 30}, radius = 60 }
      circB = Circle { centre = Vector { x = 120, y = 30}, radius = 40 }
   in noCollisionBetween circA circB

test_vertically_separated_circles_touching_exactly_do_not_overlap =
  let circA = Circle { centre = Vector { x = 20, y = 30}, radius = 60 }
      circB = Circle { centre = Vector { x = 20, y = 130}, radius = 40 }
   in noCollisionBetween circA circB

test_horizontally_separated_with_sum_of_radii_just_exceeding_separation =
  let circA = Circle { centre = Vector { x = 20, y = 30}, radius = 60 }
      circB = Circle { centre = Vector { x = 120, y = 30}, radius = 41 }
   in collisionBetween circA circB

test_vertically_separated_with_sum_of_radii_just_exceeding_separation =
  let circA = Circle { centre = Vector { x = 20, y = 30}, radius = 60 }
      circB = Circle { centre = Vector { x = 20, y = 130}, radius = 41 }
   in collisionBetween circA circB

test_diagonally_touching_exactly_less_than_sum_of_radii_on_any_one_axis =
  let circA = Circle { centre = Vector { x = 0, y = 0}, radius = 20 }
      circB = Circle { centre = Vector { x = 30, y = 40}, radius = 30 }
   in noCollisionBetween circA circB

test_diagonally_overlapping_less_than_sum_of_radii_on_any_one_axis =
  let circA = Circle { centre = Vector { x = 0, y = 0}, radius = 21 }
      circB = Circle { centre = Vector { x = 30, y = 40}, radius = 30 }
   in collisionBetween circA circB

test_non_overlapping_circles_dont_push_out =
  let circA = Circle {centre = Vector {x = 0, y = 0}, radius = 10}
      circB = Circle { centre = Vector {x = 20, y = 0}, radius = 5}
   in assertEqual (circA !!> circB) Nothing

test_horizontally_overlapping_circles =
  let circA = Circle {centre = Vector {x = 0, y = 0}, radius = 10}
      circB = Circle { centre = Vector {x = 20, y = 0}, radius = 15}
   in assertEqual (circA !!> circB) (Just (move_right 5))

test_vertically_overlapping_circles =
  let circA = Circle {centre = Vector {x = 0, y = 0}, radius = 10}
      circB = Circle { centre = Vector {x = 0, y = 20}, radius = 15}
   in assertEqual (circA !!> circB) (Just (move_up 5))

prop_colliding_shapes_have_pushout a b
 | a !!! b   = isJust (a !!> b)
 | otherwise = isNothing (a !!> b)

prop_collision_is_commutative a b
 | a !!! b   = b !!! a
 | otherwise = not (b !!! a)

prop_pushout_exists_when_collision a b = case a !!> b of
 (Just _)  -> a !!! b
 (Nothing) -> not (a !!! b)

prop_pushout_is_inversely_commutative a b = (a !!> b) == (negate <$> (b !!> a))

-- floating point means sometimes we have a tiny pushout
prop_pushout_leaves_shapes_not_colliding a b = case a !!> b of
 (Just v)  -> maybe 0 magnitude (a !!> move b v) < (magnitude v) * 0.00001
 (Nothing) -> not (a !!! b)

instance Arbitrary Shape where
 arbitrary = generateCircle
