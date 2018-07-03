module World.CreateWorld where

import Graphics.Gloss (white, yellow)
import Graphics.Gloss.Interface.IO.Game (Key (Char))

import Entities.Block
import Entities.Jumpman
import Entities.Coin

import World.Assets
import Systems.Controller
import Shapes.Shape
import World.World


walls :: [Block]
walls = [ Block (rectangle (-310) 310 230 220) white
        , Block (rectangle (-310) 310 (-220) (-230)) white
        , Block (rectangle (-310) (-300) (-230) 230) white
        , Block (rectangle 300 310 (-230) 230) white
        -- platforms
        , Block (rectangle (-210) (-130) (-110) (-120)) white
        , Block (rectangle 130 210 (-110) (-120)) white
        , Block (rectangle (-210) (-130) 120 110) white
        , Block (rectangle 130 210 120 110) white
        , Block (rectangle (-40) 40 (-10) (-20)) white
        , Block (polygon [((-300), (-220)), ((-300), (-120)), ((-100), (-220))]) white
        , Block (polygon [((300), (-220)), ((300), (-120)), ((100), (-220))]) white
        ]

createWorld :: Assets -> World
createWorld assets = World { _scenery = walls
                           , _jumpman = mkJumpman (0, 0) (withKeys (Char 'z') (Char 'x') (Char '/'))
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
