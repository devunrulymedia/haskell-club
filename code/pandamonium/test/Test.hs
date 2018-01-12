{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} CircleCollisions
import {-@ HTF_TESTS @-} RectangleCollisions
import {-@ HTF_TESTS @-} ShapeCollisions

main = htfMain htf_importedTests
