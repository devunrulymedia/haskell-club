module Pandamonium.World.Stage where

import Pandamonium.Entities.Block
import Pandamonium.Entities.Coin
import Graphics.Gloss.Data.Vector

data Stage = Stage [ Block ] [ Coin ] Vector
