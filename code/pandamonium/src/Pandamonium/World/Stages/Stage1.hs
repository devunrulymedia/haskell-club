module Pandamonium.World.Stages.Stage1 where

import Common.Entities.Entity
import Common.Entities.Block
import Common.Shapes.Shape
import Pandamonium.Entities.EntityTypes
import Pandamonium.Entities.Coin
import Pandamonium.World.Stage
import Graphics.Gloss (white)

walls = [ Entity EBlock 0 $ Block (rectangle (-710) 710 430 420) white
        , Entity EBlock 1 $ Block (rectangle (-710) 710 (-420) (-430)) white
        , Entity EBlock 2 $ Block (rectangle (-710) (-700) (-430) 430) white
        , Entity EBlock 3 $ Block (rectangle 700 710 (-430) 430) white
        -- platforms
        , Entity EBlock 4 $ Block (rectangle (-210) (-130) (-110) (-120)) white
        , Entity EBlock 5 $ Block (rectangle 130 210 (-110) (-120)) white
        , Entity EBlock 6 $ Block (rectangle (-210) (-130) 120 110) white
        , Entity EBlock 7 $ Block (rectangle 130 210 120 110) white
        , Entity EBlock 8 $ Block (rectangle (-40) 40 (-10) (-20)) white
        , Entity EBlock 9 $ Block (polygon [((-700), (-420)), ((-700), (-120)), ((-100), (-420))]) white
        , Entity EBlock 10 $ Block (rectangle 200 210 (-120) 120) white
        ]

coins = [ Entity ECoin 11 $ Coin (-170, -70)
        , Entity ECoin 12 $ Coin (170, -70)
        , Entity ECoin 13 $ Coin (-170, 160)
        , Entity ECoin 14 $ Coin (170, 160)
        , Entity ECoin 15 $ Coin (0, 30)
        , Entity ECoin 16 $ Coin (0, -400)
        ]

stage1 :: Stage
stage1 = Stage walls coins (0, 0)
