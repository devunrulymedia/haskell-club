module Generators where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Gen
import Vector
import Shape

instance Arbitrary Vector where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return Vector { x = x, y = y}

instance Arbitrary Shape where
  arbitrary = oneof [rectangle, circle] where
    rectangle = do
      top <- arbitrary
      bottom <- arbitrary `suchThat` (< top)
      left <- arbitrary
      right <- arbitrary `suchThat` (> left)
      return Rectangle { left = left, right = right, top = top, bottom = bottom }
    circle = do
      centre <- arbitrary
      Positive radius <- arbitrary
      return Circle { centre = centre, radius = radius }
