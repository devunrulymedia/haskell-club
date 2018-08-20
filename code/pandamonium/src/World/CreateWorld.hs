module World.CreateWorld where

import Graphics.Gloss (white, yellow)
import Graphics.Gloss.Interface.IO.Game (Key (Char))

import Entities.Block
import Entities.Panda
import Entities.Coin

import World.Assets
import Systems.Controller
import Shapes.Shape
import World.World
import World.Stage

createWorld :: Assets -> Stage -> World
createWorld assets (Stage ws cs spawn) = World
  { _scenery = ws
  , _panda = mkPanda (pandas assets) spawn (withKeys (Char 'z') (Char 'x') (Char '/'))
  , _coins = cs
  , _score = 0
  , _numbers = numberSprites assets
  }
