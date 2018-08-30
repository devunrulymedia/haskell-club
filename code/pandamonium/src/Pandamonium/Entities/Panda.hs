{-# LANGUAGE TemplateHaskell #-}

module Pandamonium.Entities.Panda (Panda, mkPanda, pandaRedux) where

import Control.Lens
import Control.Arrow
import Control.Monad
import Common.Shapes.Shape
import Common.Renderable
import Common.Redux
import Common.Physics.Physics
import Common.Physics.Collisions
import Pandamonium.Systems.Controller
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Pandamonium.Game.GameEvent
import Pandamonium.Entities.EntityTypes
import Pandamonium.Entities.Panda.Jump
import Pandamonium.Entities.Panda.Run
import Pandamonium.Entities.Panda.Collisions
import Pandamonium.Entities.Panda.Panda
import Pandamonium.Entities.Panda.Animation

instance Shaped Panda where
  shape pd = let (x, y) = pd ^. pos in rectangle (x-48) (x+48) (y-36) (y+36)

scoreCoin :: Collision EntityType Integer -> Panda -> IOEvents Panda
scoreCoin (Collision EPanda _ ECoin _ _) panda = do fireEvent (PointsScored 5); return panda
scoreCoin _ panda = return panda

updatePanda :: Float -> Panda -> Events Panda
updatePanda t pd = return pd
               <&> moveHorizontally t
               <&> ascend t
               <&> capSpeed
               <&> setFacing
               <&> gravitate t
               <&> integrate t

reducePanda :: GameEvent -> Panda -> IOEvents Panda
reducePanda e pd = return pd
               <&> state %~ handleCollisions e
               <&> triggerJump e

otherThing :: Collision EntityType Integer -> Panda -> IOEvents Panda
otherThing c pd = return pd
              <&> state %~ processCollisions c
              >>= scoreCoin c

listenPanda :: Event -> Panda -> Events Panda
listenPanda e pd = return pd
               >>= controller %%~ updateController e

pandaRedux :: Redux Panda
pandaRedux = Redux
  { reducer  = composeHandler [ concrify reducePanda, concrify otherThing ]
  , updater  = updatePanda
  , listener = listenPanda
  }
