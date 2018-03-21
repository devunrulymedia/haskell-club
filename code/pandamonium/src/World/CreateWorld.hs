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
    { _paddle = Paddle (-150, 0) (Rectangle (-5) 5 25 (-25)) 200 (withKeys (Char 'a') (Char 'z'))
    , _score = 0
    , _scoreLocation = (-120, 125)
    , _hue = orange
    , _endzone = Block (Rectangle 190 200 100 (-100)) orange
    , _playerNumber = 1
    }
  , Player
    { _paddle = Paddle (150, 0)  (Rectangle (-5) 5 25 (-25)) 200 (withKeys (Char '\'') (Char '/'))
    , _score = 0
    , _scoreLocation = (80, 125)
    , _hue = blue
    , _endzone = Block (Rectangle (-200) (-190) 100 (-100)) blue
    , _playerNumber = 2
    }
  ]


createWorld :: Assets -> World
createWorld assets = let initBall = Ball (20, 0) (200, 300) (ballSprite assets)
                      in World { _scenery = walls
                               , _ball = initBall
                               , _initialBall = initBall
                               , _events = []
                               , _players = playerList
                               }
