module Entities.Panda.Movement
  ( module Entities.Panda.Collisions
  , module Entities.Panda.MovementStateMachine
  ) where

import Entities.Panda.Panda
import Entities.Panda.Collisions
import Entities.Panda.MovementStateMachine
import Systems.Controller
import Control.Lens

continueJump :: Float -> Float -> Panda -> Panda
continueJump t f pd = case toJoypad $ pd ^. controller of
  (Joypad _ Pressed)  -> pd
  (Joypad _ Released) -> state .~ Falling $ pd

continueWallJump :: Float -> Direction -> Float -> Panda -> Panda
continueWallJump t d f pd = pd

ascend :: Float -> Panda -> Panda
ascend t pd = case pd ^. state of
  (Jumping f)       -> continueJump t f pd
  (WallJumping d f) -> continueWallJump t d f pd
  otherwise         -> pd
