module Pandamonium.World.Stage where

import Graphics.Gloss.Data.Vector
import Common.Entities.Entity
import Common.Entities.Block
import Pandamonium.Entities.EntityTypes
import Pandamonium.Entities.Coin

data Stage = Stage [ Block ] [ Coin ] Vector
