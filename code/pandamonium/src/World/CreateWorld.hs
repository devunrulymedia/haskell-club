module World.CreateWorld where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Entities.Block
import Entities.Ball
import Entities.Player
import Entities.Paddle

import World.Assets
import Systems.Controller
import Shapes.Shape
import World.World


walls :: [Block]
walls = [ Block (Rectangle (-200) 200 100 90) white
        , Block (Rectangle (-200) 200 (-90) (-100)) white
        , Block (Rectangle (-5) 5 (-40) (-90)) white ]

playerList :: [Player]
playerList =
  [ Player
    { paddle = Paddle (-150, 0) (Rectangle (-5) 5 25 (-25)) 200 (withKeys (Char 'a') (Char 'z'))
    , score = 0
    , scoreLocation = (-120, 125)
    , hue = orange
    , endzone = Block (Rectangle 190 200 100 (-100)) orange
    , index = 1
    }
  , Player
    { paddle = Paddle (150, 0)  (Rectangle (-5) 5 25 (-25)) 200 (withKeys (Char '\'') (Char '/'))
    , score = 0
    , scoreLocation = (80, 125)
    , hue = blue
    , endzone = Block (Rectangle (-200) (-190) 100 (-100)) blue
    , index = 2
    }
  ]


createWorld :: Assets -> World
createWorld assets = let initBall = Ball (20, 0) (200, 300) (ballSprite assets)
                      in World { scenery = walls
                               , ball = initBall
                               , initialBall = initBall
                               , events = []
                               , players = playerList
                               }
