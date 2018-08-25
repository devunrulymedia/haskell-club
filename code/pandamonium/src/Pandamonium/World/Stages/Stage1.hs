module Pandamonium.World.Stages.Stage1 where

import Common.Entities.Block
import Common.Shapes.Shape
import Pandamonium.Entities.Coin
import Pandamonium.World.Stage
import Graphics.Gloss (white)

walls = [ Block (rectangle (-710) 710 430 420) white
        , Block (rectangle (-710) 710 (-420) (-430)) white
        , Block (rectangle (-710) (-700) (-430) 430) white
        , Block (rectangle 700 710 (-430) 430) white
        -- platforms
        , Block (rectangle (-210) (-130) (-110) (-120)) white
        , Block (rectangle 130 210 (-110) (-120)) white
        , Block (rectangle (-210) (-130) 120 110) white
        , Block (rectangle 130 210 120 110) white
        , Block (rectangle (-40) 40 (-10) (-20)) white
        , Block (polygon [((-700), (-420)), ((-700), (-120)), ((-100), (-420))]) white
        , Block (rectangle 200 210 (-120) 120) white
        ]

coins = [ Coin "coin1" (-170, -70)
        , Coin "coin2" (170, -70)
        , Coin "coin3" (-170, 160)
        , Coin "coin4" (170, 160)
        , Coin "coin5" (0, 30)
        ]

stage1 :: Stage
stage1 = Stage walls coins (0, 0)
