module Pandamonium.World.Stages.Stage2 where

import Graphics.Gloss (white)

import Common.Shapes.Shape
import Common.Entities.Entity
import Common.Entities.Block

import Pandamonium.Entities.EntityTypes
import Pandamonium.Entities.Coin
import Pandamonium.World.Stage

walls = [ Block (rectangle (-710) 710 430 420) white
        , Block (rectangle (-710) 710 (-420) (-430)) white
        , Block (rectangle (-710) (-700) (-430) 430) white
        , Block (rectangle 700 710 (-430) 430) white
        ]

coins = [ Coin (-200, -325)
        , Coin (-100, -235)
        , Coin (0, -200)
        , Coin (100, -235)
        , Coin (200, -325)
        , Coin (0, -400)
        ]

stage2 :: Stage
stage2 = Stage walls coins ((-600), (-380))
