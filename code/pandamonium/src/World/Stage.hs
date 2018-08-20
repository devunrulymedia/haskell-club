module World.Stage where

import Entities.Block
import Entities.Coin
import Graphics.Gloss.Data.Vector

data Stage = Stage [ Block ] [ Coin ] Vector
