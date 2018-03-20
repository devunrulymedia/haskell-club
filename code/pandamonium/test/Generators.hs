module Generators where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Gen
import Shape

instance Arbitrary DeRectangle where
  arbitrary = do
    top <- arbitrary
    bottom <- arbitrary `suchThat` (< top)
    left <- arbitrary
    right <- arbitrary `suchThat` (> left)
    return $ DeRectangle left right top bottom

instance Arbitrary DeCircle where
  arbitrary = do
    centre <- arbitrary
    Positive radius <- arbitrary
    return $ DeCircle centre radius

instance Arbitrary Shape where
  arbitrary = oneof [construct <$> Left <$> arbitrary, construct <$> Right <$> arbitrary]
