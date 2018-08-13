module Entities.Panda.Run where

import Control.Lens
import Entities.Panda.Panda
import Graphics.Gloss.Data.Vector
import Shapes.Movables
import Systems.Controller

hv :: Float
hv = 1500

reverseBoost :: Float
reverseBoost = 2.5

moveHorizontally :: Float -> Panda -> Panda
moveHorizontally t pd = let (x, y) = velocity pd in case pd ^. controller of
  (Controller (ControlState True False _) _) -> applyImpulse (t*(-hv)*(if x > 0 then reverseBoost else 1), 0) $ pd
  (Controller (ControlState False True _) _) -> applyImpulse (t*hv*(if x < 0 then reverseBoost else 1), 0) $ pd
  otherwise -> vel %~ (slowBy hv) $ pd where
    slowBy :: Float -> Vector -> Vector
    slowBy f (x, y)
      | x > 0 = (x - (min x (t*f)), y)
      | True = (x + (min (-x) (t*f)), y)
