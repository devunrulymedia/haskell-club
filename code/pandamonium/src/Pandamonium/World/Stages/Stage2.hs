module Pandamonium.World.Stages.Stage2 where

import Graphics.Gloss (white)

import Common.Shapes.Shape
import Common.Entities.Entity
import Common.Entities.Block

import Pandamonium.Entities.EntityTypes
import Pandamonium.Entities.Coin
import Pandamonium.World.Stage

walls = [ Entity EBlock 0 $ Block (rectangle (-710) 710 430 420) white
        , Entity EBlock 1 $ Block (rectangle (-710) 710 (-420) (-430)) white
        , Entity EBlock 2 $ Block (rectangle (-710) (-700) (-430) 430) white
        , Entity EBlock 3 $ Block (rectangle 700 710 (-430) 430) white
        ]

coins = [ Entity ECoin 4 $ Coin "coin1" (-200, -325)
        , Entity ECoin 5 $ Coin "coin2" (-100, -235)
        , Entity ECoin 6 $ Coin "coin3" (0, -200)
        , Entity ECoin 7 $ Coin "coin4" (100, -235)
        , Entity ECoin 8 $ Coin "coin5" (200, -325)
        ]

stage2 :: Stage
stage2 = Stage walls coins ((-600), (-380))
