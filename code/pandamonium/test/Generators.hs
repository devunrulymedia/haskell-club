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

instance Arbitrary Rectangle where
  arbitrary = do
    top <- arbitrary
    bottom <- arbitrary `suchThat` (< top)
    left <- arbitrary
    right <- arbitrary `suchThat` (> left)
    return $ Rectangle left right top bottom

instance Arbitrary Circle where
  arbitrary = do
    centre <- arbitrary
    Positive radius <- arbitrary
    return $ Circle centre radius

instance Arbitrary Shape where
  arbitrary = oneof [Rect <$> arbitrary, Circ <$> arbitrary]
