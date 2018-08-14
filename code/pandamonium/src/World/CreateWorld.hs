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


walls :: [Block]
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

createWorld :: Assets -> World
createWorld assets = World { _scenery = walls
                           , _panda = mkPanda (pandas assets) (0, 0) (withKeys (Char 'z') (Char 'x') (Char '/'))
                           , _coins =
                             [ Coin "coin1" (-170, -70)
                             , Coin "coin2" (170, -70)
                             , Coin "coin3" (-170, 160)
                             , Coin "coin4" (170, 160)
                             , Coin "coin5" (0, 30)
                             ]
                           , _score = 0
                           , _numbers = numberSprites assets
                           }
