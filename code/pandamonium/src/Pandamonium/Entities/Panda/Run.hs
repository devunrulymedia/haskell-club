module Pandamonium.Entities.Panda.Run where

import Control.Lens
import Pandamonium.Entities.Panda.Panda
import Pandamonium.Entities.Panda.MovementStateMachine
import Graphics.Gloss.Data.Vector
import Pandamonium.Shapes.Movables
import Pandamonium.Systems.Controller

hv :: Float
hv = 1500

reverseBoost :: Float
reverseBoost = 2.5

moveHorizontally :: Float -> Panda -> Panda
moveHorizontally t pd = let (x, y) = velocity pd in case (toJoypad $ pd ^. controller) of
  (Joypad JLeft _)    -> applyImpulse (t*(-hv)*(if x > 0 then reverseBoost else 1), 0) $ pd
  (Joypad JRight _)   -> applyImpulse (t*hv*(if x < 0 then reverseBoost else 1), 0) $ pd
  (Joypad JNeutral _) -> vel %~ (slowBy hv) $ pd where
    slowBy :: Float -> Vector -> Vector
    slowBy f (x, y)
      | x > 0 = (x - (min x (t*f)), y)
      | True = (x + (min (-x) (t*f)), y)

hlimit :: Float -> Vector -> Vector
hlimit mx (x, y)
  | x > mx = (mx, y)
  | x < (-mx) = (-mx, y)
  | otherwise = (x, y)

capSpeed :: Panda -> Panda
capSpeed pd = vel %~ hlimit 600 $ pd

setFacing :: Panda -> Panda
setFacing pd = setFacing' (pd ^. state) (pd ^. vel) where
  setFacing' Grounded (x, _)
    | x < 0 = facing .~ DLeft $ pd
    | x > 0 = facing .~ DRight $ pd
    | otherwise = pd
  setFacing' _ _ = pd