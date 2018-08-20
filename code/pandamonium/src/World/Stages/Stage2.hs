module World.Stages.Stage2 where

import Entities.Block
import Entities.Coin
import World.Stage
import Shapes.Shape
import Graphics.Gloss (white)

walls = [ Block (rectangle (-710) 710 430 420) white
        , Block (rectangle (-710) 710 (-420) (-430)) white
        , Block (rectangle (-710) (-700) (-430) 430) white
        , Block (rectangle 700 710 (-430) 430) white
        ]

coins = [ Coin "coin1" (-170, -70)
        , Coin "coin2" (170, -70)
        , Coin "coin3" (-170, 160)
        , Coin "coin4" (170, 160)
        , Coin "coin5" (0, 30)
        ]

stage2 :: Stage
stage2 = Stage walls coins ((-600), (-380))
