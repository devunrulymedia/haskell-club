module Main where

import Test.HUnit
import qualified ShapeTest

main :: IO ()
main = do runTestTT ShapeTest.tests
          return ()
