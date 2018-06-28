{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Shapes.CollisionAssertions where

import Test.Framework
import Control.Monad
import Shapes.Shape

-- class Approximate a where
--   nearlyIs :: a -> a -> Bool
--
-- instance Approximate Float where
--   nearlyIs = a / b > 0.9999 && b / a > 0.9999
--
-- instance Approximate Vector where
--   n

collisionBetween a b = unless (a !!! b) (assertFailure msg)
 where msg = "\n* expected: " ++ show a ++ " to collide with " ++ show b ++ "\n* but got: no collision"

noCollisionBetween a b = when (a !!! b) (assertFailure msg)
 where msg = "\n* expected: " ++ show a ++ " to not collide with " ++ show b ++ "\n* but got: a collision"

-- assertNearlyEqual (ex,ey) (ax, ay) = unless ((ex `nearlyIs` ax) && (ey `nearlyIs` ay)) (assertFailure msg)
--   where msg ="\n* expected: " ++ show (ex, ey) ++ ", but got " ++ show (ax, ay)
