module World.CreateWorld where

import Graphics.Gloss (white, yellow)
import Graphics.Gloss.Interface.IO.Game (Key (Char))

import Entities.Block
import Entities.Thruster

import World.Assets
import Systems.Controller
import Shapes.Shape
import World.World


walls :: [Block]
walls = [ Block (rectangle (-310) 310 230 220) white
        , Block (rectangle (-310) 310 (-220) (-230)) white
        , Block (rectangle (-310) (-300) (-230) 230) white
        , Block (rectangle 300 310 (-230) 230) white
        ]

createWorld :: Assets -> World
createWorld assets = World { _scenery = walls
                           , _thruster = Thruster (0, 0) (0, 0) Falling (withKeys (Char 'z') (Char 'x') (Char '/')) (ballSprite assets)
                           }
