module Pandamonium.Entities.Coin where

import Graphics.Gloss (Color, Vector, color, yellow)
import Common.Redux
import Common.Entities.Entity
import Common.Shapes.Shape
import Common.Renderable

import Pandamonium.Entities.EntityTypes
import Pandamonium.Game.GameEvent

data Coin = Coin String Vector

instance Shaped Coin where
  shape (Coin _ centre) = circle centre 8

instance Renderable Coin where
  render coin = color yellow $ render $ shape coin

coinRedux :: Redux (Ent Coin) GameEvent
coinRedux = noOpRedux
