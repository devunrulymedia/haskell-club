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
walls = [ Block (rectangleV (-310, 230) (620, 10)) white
        , Block (rectangleV (-310, -230) (620, 10)) white
        , Block (rectangleV (-310, -230) (10, 470)) white
        , Block (rectangleV (310, -230) (10, 470)) white
        ]

surface :: [Block]
surface = [ Block (rectangleV (-300, 100) (100,2)) white ]

createWorld :: Assets -> World
createWorld assets = World { _scenery = walls ++ surface
                           , _thruster = Thruster (0, 0) (0, 0) (0, 0) (0, 1) (withKeys (Char 'z') (Char 'x') (Char 'm')) (shipSprite assets)
                           }
