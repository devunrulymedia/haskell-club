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

import Pandamonium.Entities.EntityTypes
import Pandamonium.Game.GameEvent

data Coin = Coin Vector

position :: Coin -> Vector
position (Coin pos) = pos

instance Shaped Coin where
  shape (Coin centre) = circle centre 8

instance Renderable Coin where
  render coin = color yellow $ render $ shape coin

triggerRespawn :: Collision EntityType Integer -> Ent Coin -> IOEvents (Ent Coin)
triggerRespawn (Collision ECoin coinId _ _ _) entity = if coinId == entity ^. eid
  then do fireEvent (Destroy coinId)
          awaitEvent 5 (RespawnCoin coinId (position (entity ^. edata)))
          return entity
  else return entity
triggerRespawn _ entity = return entity


coinRedux :: Redux (Ent Coin)
coinRedux = Redux
  { reducer = focusM triggerRespawn
  , updater = noOp
  , listener = noOp
  }
