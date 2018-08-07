module Entities.Panda.Movement
  ( module Entities.Panda.Collisions
  , module Entities.Panda.MovementStateMachine
  , ascend
  ) where

import Shapes.Shape
import Entities.Panda.Panda
import Entities.Panda.Collisions
import Entities.Panda.MovementStateMachine
import Systems.Controller
import Control.Lens
import Graphics.Gloss.Data.Vector

jboost :: Vector
jboost = (0, 1800)

continueJump :: Float -> Float -> Panda -> Panda
continueJump t f pd = case toJoypad $ pd ^. controller of
  (Joypad _ Released) -> state .~ Falling $ pd
  (Joypad _ Pressed)  -> if f - t < 0
                       then state .~ Falling $ pd
                       else state .~ Jumping (f - t)
                             $ applyImpulse (mulSV t jboost)
                             $ pd

wallJumpImpulse :: Direction -> Vector
wallJumpImpulse DLeft = ((-3000), 1200)
wallJumpImpulse DRight = (3000, 1200)

continueWallJump :: Float -> Direction -> Float -> Panda -> Panda
continueWallJump t d f pd = case toJoypad $ pd ^. controller of
  (Joypad _ Released) -> state .~ Falling $ pd
  (Joypad _ Pressed)  -> if f - t < 0
                       then state .~ Falling $ pd
                       else state .~ WallJumping d (f - t)
                             $ applyImpulse (mulSV t (wallJumpImpulse d))
                             $ pd

ascend :: Float -> Panda -> Panda
ascend t pd = case pd ^. state of
  (Jumping f)       -> continueJump t f pd
  (WallJumping d f) -> continueWallJump t d f pd
  otherwise         -> pd
