{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ShapeCollisions (htf_thisModulesTests) where

import Test.Framework
import Control.Monad
import Data.Maybe
import Generators
import CollisionAssertions
import Graphics.Gloss.Data.Vector
import Shape

prop_colliding_shapes_have_pushout :: Shape -> Shape -> Bool
prop_colliding_shapes_have_pushout a b
 | a !!! b   = isJust (a !!> b)
 | otherwise = isNothing (a !!> b)

prop_collision_is_commutative :: Shape -> Shape -> Bool
prop_collision_is_commutative a b
 | a !!! b   = b !!! a
 | otherwise = not (b !!! a)

prop_pushout_exists_when_collision :: Shape -> Shape -> Bool
prop_pushout_exists_when_collision a b = case a !!> b of
 (Just _)  -> a !!! b
 (Nothing) -> not (a !!! b)

prop_pushout_is_inversely_commutative :: Shape -> Shape -> Bool
prop_pushout_is_inversely_commutative a b = (a !!> b) == (negate <$> (b !!> a))

-- floating point means sometimes we have a tiny pushout
prop_pushout_leaves_shapes_not_colliding :: Shape -> Shape -> Bool
prop_pushout_leaves_shapes_not_colliding a b = case a !!> b of
 (Just v)  -> maybe 0 magV (a !!> move b v) < (magV v) * 0.00001
 (Nothing) -> not (a !!! b)
