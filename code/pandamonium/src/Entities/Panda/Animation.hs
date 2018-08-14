module Entities.Panda.Animation where

import Graphics.Gloss
import Renderable
import Entities.Panda.Panda
import Entities.Panda.MovementStateMachine
import Control.Lens

spriteFor :: Panda -> Picture
spriteFor pd = (pd ^. sprites) !! (frame $ pd ^. state) where
  frame Grounded = 1
  frame Airborne = 7
  frame (WallHugging _) = 2

instance Renderable Panda where
  render pd = let (x, y) = pd ^. pos in translate x y $ scale 2 2 $ spriteFor pd
