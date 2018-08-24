{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Shapes.CollisionAssertions where

import Test.Framework
import Control.Monad
import Thrust.Shapes.Shape

collisionBetween a b = unless (a !!! b) (assertFailure msg)
 where msg = "\n* expected: " ++ show a ++ " to collide with " ++ show b ++ "\n* but got: no collision"

noCollisionBetween a b = when (a !!! b) (assertFailure msg)
 where msg = "\n* expected: " ++ show a ++ " to not collide with " ++ show b ++ "\n* but got: a collision"
