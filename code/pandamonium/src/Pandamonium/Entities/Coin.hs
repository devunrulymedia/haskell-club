module Pandamonium.Entities.Coin where

import Graphics.Gloss (Color, Vector, color, yellow)
import Control.Lens
import Data.Dynamic

import Common.Redux
import Common.Timer
import Common.Entities.Entity
import Common.Physics.Collisions
import Common.Shapes.Shape
import Common.Renderable
import Common.Entities.Destroyer
import Common.Entities.Spawner

import Pandamonium.Entities.EntityTypes
import Pandamonium.Game.GameEvent

data Coin = Coin Vector

position :: Coin -> Vector
position (Coin pos) = pos

instance Shaped Coin where
  shape (Coin centre) = circle centre 8

instance Renderable Coin where
  render coin = color yellow $ render $ shape coin

pickup :: Collision EntityType Integer -> Ent Coin -> IOEvents (Ent Coin)
pickup (Collision ECoin coinId _ _ _) entity = if coinId == entity ^. eid
  then do fireEvent (Destroy coinId)
          spawnIn 5 (entity ^. edata)
          return entity
  else return entity
pickup _ entity = return entity

coinRedux :: Redux (Ent Coin)
coinRedux = Redux
  { reducer = focusM pickup
  , updater = noOp
  , listener = noOp
  }
