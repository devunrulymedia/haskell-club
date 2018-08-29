module Pandamonium.Entities.Coin where

import Graphics.Gloss (Color, Vector, color, yellow)
import Control.Lens

import Common.Redux
import Common.Entities.Entity
import Common.Shapes.Shape
import Common.Renderable

import Pandamonium.Entities.EntityTypes
import Pandamonium.Game.GameEvent

data Coin = Coin String Vector

position :: Coin -> Vector
position (Coin _ pos) = pos

instance Shaped Coin where
  shape (Coin _ centre) = circle centre 8

instance Renderable Coin where
  render coin = color yellow $ render $ shape coin

triggerRespawn :: GameEvent -> Ent Coin -> IOEvents GameEvent (Ent Coin)
triggerRespawn (Collision ECoin coinId _ _ _) entity = if coinId == entity ^. eid
  then do fireEvent2 (Trigger 5 (RespawnCoin coinId (position (entity ^. edata))))
          return entity
  else return entity
triggerRespawn _ entity = return entity


coinRedux :: Redux (Ent Coin) GameEvent
coinRedux = Redux
  { reducer = triggerRespawn
  , updater = noOp
  , listener = noOp
  }
