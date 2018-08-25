module Pandamonium.World.CreateWorld where

import Graphics.Gloss (white, yellow)
import Graphics.Gloss.Interface.IO.Game (Key (Char))

import Common.Entities.Block

import Pandamonium.Entities.Panda
import Pandamonium.Entities.Coin
import Pandamonium.World.Assets
import Pandamonium.Systems.Controller
import Pandamonium.World.World
import Pandamonium.World.Stage

createWorld :: Assets -> Stage -> World
createWorld assets (Stage ws cs spawn) = World
  { _scenery = ws
  , _panda = mkPanda (pandas assets) spawn (withKeys (Char 'z') (Char 'x') (Char '/'))
  , _coins = cs
  , _score = 0
  , _numbers = numberSprites assets
  }
