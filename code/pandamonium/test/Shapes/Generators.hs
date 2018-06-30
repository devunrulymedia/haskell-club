module Shapes.Generators where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Gen
import Shapes.Shape

arbitraryRect :: Gen Shape
arbitraryRect = do
    top <- arbitrary
    bottom <- arbitrary `suchThat` (< top)
    left <- arbitrary
    right <- arbitrary `suchThat` (> left)
    return $ rectangle left right top bottom

arbitraryCircle :: Gen Shape
arbitraryCircle = do
    centre <- arbitrary
    Positive radius <- arbitrary
    return $ circle centre radius

instance Arbitrary Shape where
  arbitrary = oneof [arbitraryRect, arbitraryCircle]
