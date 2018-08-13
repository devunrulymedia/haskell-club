{-# LANGUAGE TemplateHaskell #-}


module Entities.Panda (Panda, mkPanda, pandaRedux) where

import Control.Lens
import Control.Arrow
import Control.Monad
import Shapes.Shape
import Renderable
import Redux
import Systems.Controller
import Systems.Physics
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Game.GameEvent
import Entities.Panda.Jump
import Entities.Panda.Collisions
import Entities.Panda.Panda

hv :: Float
hv = 1500

reverseBoost :: Float
reverseBoost = 2.5

jfuel :: Float
jfuel = 0.2

instance Shaped Panda where
  shape pd = let (x, y) = pd ^. pos in rectangle (x-24) (x+24) (y-18) (y+18)

instance Renderable Panda where
  render pd = let (x, y) = pd ^. pos in translate x y $ scale 2 2 $ pd ^. sprite

hlimit :: Float -> Vector -> Vector
hlimit mx (x, y)
  | x > mx = (mx, y)
  | x < (-mx) = (-mx, y)
  | otherwise = (x, y)

collectEvents :: Event -> Panda -> Events GameEvent Panda
collectEvents event pd = controller %%~ updateController event $ pd

capSpeed :: Panda -> Panda
capSpeed pd = vel %~ hlimit 600 $ pd

pandaHandle :: GameEvent -> Panda -> Panda
pandaHandle (JumpPressed) = jump
pandaHandle _ = id

moveHorizontally :: Float -> Panda -> Panda
moveHorizontally t pd = let (x, y) = velocity pd in case pd ^. controller of
  (Controller (ControlState True False _) _) -> applyImpulse (t*(-hv)*(if x > 0 then reverseBoost else 1), 0) $ pd
  (Controller (ControlState False True _) _) -> applyImpulse (t*hv*(if x < 0 then reverseBoost else 1), 0) $ pd
  otherwise -> vel %~ (slowBy hv) $ pd where
    slowBy :: Float -> Vector -> Vector
    slowBy f (x, y)
      | x > 0 = (x - (min x (t*f)), y)
      | True = (x + (min (-x) (t*f)), y)

updatePanda :: Float -> Panda -> Events GameEvent Panda
updatePanda t pd = return pd
               <&> moveHorizontally t
               <&> ascend t
               <&> capSpeed
               <&> gravitate t
               <&> integrate t

reducePanda :: GameEvent -> Panda -> IOEvents GameEvent Panda
reducePanda e pd = return pd
               <&> state %~ handleCollisions e
               <&> pandaHandle e

listenPanda :: Event -> Panda -> Events GameEvent Panda
listenPanda e pd = return pd
               >>= collectEvents e

pandaRedux :: Redux Panda GameEvent
pandaRedux = Redux
  { reducer  = reducePanda
  , updater  = updatePanda
  , listener = listenPanda
  }
