{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Shapes.CircleCollisions
import {-@ HTF_TESTS @-} Shapes.RectangleCollisions
import {-@ HTF_TESTS @-} Shapes.PolygonCollisions
import {-@ HTF_TESTS @-} Redux.ReduxTest
import {-@ HTF_TESTS @-} Redux.TimerTest
import {-@ HTF_TESTS @-} Components.ComponentTest

main = htfMain htf_importedTests
