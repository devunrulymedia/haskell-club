module World.CreateWorld where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Entities.Block
import Entities.Jumpman

import World.Assets
import Systems.Controller
import Shapes.Shape
import World.World


walls :: [Block]
walls = [ Block (rectangle (-310) 310 230 220) white
        , Block (rectangle (-310) 310 (-220) (-230)) white
        , Block (rectangle (-310) (-300) (-230) 230) white
        , Block (rectangle 300 310 (-230) 230) white
        , Block (rectangle (-210) (-130) (-110) (-120)) white
        , Block (rectangle 130 210 (-110) (-120)) white
        , Block (rectangle (-210) (-130) 120 110) white
        , Block (rectangle 130 210 120 110) white
        , Block (rectangle (-40) 40 (-10) (-20)) white
        ]

createWorld :: Assets -> World
createWorld assets = World { _scenery = walls
                           , _jumpman = Jumpman (0, 0) (0, 0) Falling (withKeys (Char 'z') (Char 'x') (Char '/'))
                           }
