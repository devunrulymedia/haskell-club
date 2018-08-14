module Entities.Panda.Jump (ascend, jump) where

import Shapes.Shape
import Entities.Panda.Panda
import Entities.Panda.Collisions
import Entities.Panda.MovementStateMachine
import Systems.Controller
import Control.Lens
import Graphics.Gloss.Data.Vector

jboost :: Vector
jboost = (0, 1800)

wboost :: Direction -> Vector
wboost d = (pushOff d 3000, 1800)

jump_power :: Float
jump_power = 500

walljump_power :: Float
walljump_power = 350

extra_lift_duration :: Float
extra_lift_duration = 0.2

jump :: Panda -> Panda
jump pd = case (pd ^. state, pd ^. vel) of
  (Grounded, (vx, vy)) -> vel .~ (vx, jump_power)
                        $ state .~ Airborne
                        $ impulse .~ Just (Impulse extra_lift_duration jboost)
                        $ pd
  (WallHugging d, (vx, vy)) -> vel .~ (pushOff d 1500, walljump_power)
                             $ state .~ Airborne
                             $ impulse .~ Just (Impulse extra_lift_duration (wboost d))
                             $ pd
  otherwise -> pd

ascend :: Float -> Panda -> Panda
ascend t pd = cj (toJoypad $ pd ^. controller) (pd ^. impulse) where
  cj (Joypad _ Pressed) (Just (Impulse f v)) =
    let remainingFuel = f -t
        newImpulse = if remainingFuel > 0
                   then Just (Impulse remainingFuel v)
                   else Nothing
     in impulse .~ newImpulse
      $ applyImpulse (mulSV t v)
      $ pd
  cj _ _ = pd
