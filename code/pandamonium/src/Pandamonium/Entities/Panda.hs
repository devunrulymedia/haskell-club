{-# LANGUAGE TemplateHaskell #-}

module Pandamonium.Entities.Panda (Panda, mkPanda, pandaRedux) where

import Control.Lens
import Control.Arrow
import Control.Monad
import Pandamonium.Shapes.Shape
import Pandamonium.Renderable
import Pandamonium.Redux
import Pandamonium.Systems.Controller
import Pandamonium.Systems.Physics
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Pandamonium.Game.GameEvent
import Pandamonium.Entities.Panda.Jump
import Pandamonium.Entities.Panda.Run
import Pandamonium.Entities.Panda.Collisions
import Pandamonium.Entities.Panda.Panda
import Pandamonium.Entities.Panda.Animation

instance Shaped Panda where
  shape pd = let (x, y) = pd ^. pos in rectangle (x-48) (x+48) (y-36) (y+36)

pandaHandle :: GameEvent -> Panda -> Panda
pandaHandle (JumpPressed) = jump
pandaHandle _ = id

updatePanda :: Float -> Panda -> Events GameEvent Panda
updatePanda t pd = return pd
               <&> moveHorizontally t
               <&> ascend t
               <&> capSpeed
               <&> setFacing
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
