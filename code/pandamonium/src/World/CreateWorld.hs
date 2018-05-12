module World.CreateWorld where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Entities.Block
import Entities.Ball
import Entities.Player
import Entities.Paddle
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
        ]

createWorld :: Assets -> World
createWorld assets = let initBall = Ball (20, 0) (200, 300) (ballSprite assets)
                      in World { _scenery = walls
                               , _events = []
                               , _jumpman = Jumpman (0, 0) (0, 0) Aerial (withKeys (Char 'z') (Char 'x'))
                               }
