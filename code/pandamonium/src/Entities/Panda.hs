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
import Entities.Panda.Run
import Entities.Panda.Collisions
import Entities.Panda.Panda
import Entities.Panda.Animation

instance Shaped Panda where
  shape pd = let (x, y) = pd ^. pos in rectangle (x-24) (x+24) (y-18) (y+18)

pandaHandle :: GameEvent -> Panda -> Panda
pandaHandle (JumpPressed) = jump
pandaHandle _ = id

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
               >>= controller %%~ updateController e

pandaRedux :: Redux Panda GameEvent
pandaRedux = Redux
  { reducer  = reducePanda
  , updater  = updatePanda
  , listener = listenPanda
  }
