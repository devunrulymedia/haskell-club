module Thrust.World.CreateWorld where

import Graphics.Gloss (white, yellow)
import Graphics.Gloss.Interface.IO.Game (Key (Char))

import Thrust.Entities.Block
import Thrust.Entities.Thruster

import Thrust.World.Assets
import Thrust.Systems.Controller
import Thrust.Shapes.Shape
import Thrust.World.World

walls :: [Block]
walls = [ Block (rectangleV (-310, 230) (620, 10)) white
        , Block (rectangleV (-310, -230) (620, 10)) white
        , Block (rectangleV (-310, -230) (10, 470)) white
        , Block (rectangleV (310, -230) (10, 470)) white
        ]

surface :: [Block]
surface = [ Block (rectangleV (-300, 100) (100, 2)) white ]

createWorld :: Assets -> World
createWorld assets = World { _scenery = walls ++ surface
                           , _thruster = Thruster (0, 0) (0, 0) (0, 0) (0, 1) (withKeys (Char 'z') (Char 'x') (Char 'm')) (shipSprite assets)
                           }
