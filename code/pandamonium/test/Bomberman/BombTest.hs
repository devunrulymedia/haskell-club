{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Bomberman.BombTest (htf_thisModulesTests) where

import Test.Framework
import Bomberman.Bomb

test_align_bomb_to_grid = do
  let alignedPosition = alignToGrid (0, 0) (10, 10) (23, 27)
  assertEqual alignedPosition (20, 30)

test_align_bomb_to_grid_large_corner = do
  let alignedPosition = alignToGrid (1000, 1000) (10, 10) (23, 27)
  assertEqual alignedPosition (20, 30)
