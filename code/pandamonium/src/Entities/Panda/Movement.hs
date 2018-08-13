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

continueJump :: Float -> Panda -> Panda
continueJump t pd = cj (toJoypad $ pd ^. controller) (pd ^. impulse) where
  cj (Joypad _ Released) _         = state .~ Falling $ pd
  cj _ Nothing                     = state .~ Falling $ pd
  cj (Joypad _ Pressed) (Just (Impulse f v)) =
    let remainingFuel = f -t
        newImpulse = if remainingFuel > 0
                   then Just (Impulse remainingFuel v)
                   else Nothing
     in impulse .~ newImpulse
      $ applyImpulse (mulSV t v)
      $ pd

ascend :: Float -> Panda -> Panda
ascend t pd = case pd ^. state of
  Jumping           -> continueJump t pd
  otherwise         -> pd
