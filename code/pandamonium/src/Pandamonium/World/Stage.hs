module Pandamonium.World.Stage where

import Graphics.Gloss.Data.Vector
import Common.Entities.Block
import Pandamonium.Entities.Coin

data Stage = Stage [ Block ] [ Coin ] Vector
