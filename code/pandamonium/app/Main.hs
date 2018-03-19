module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Codec.BMP
import World
import Block
import Ball
import Renderable
import Updatable
import GameEvent
import Shape
import Paddle
import Score
import Player
import Controller

window :: Display
window = InWindow "A window!" (500, 500) (100, 100)

background :: Color
background = black

fps :: Int
fps = 600

scene :: [Block]
scene = [ Block (Rect (Rectangle (-200) 200 100 90)) white
        , Block (Rect (Rectangle (-200) 200 (-90) (-100))) white
        , Block (Rect (Rectangle (-5) 5 (-40) (-90))) white ]

p1paddle = Paddle (-150, 0) (Rect (Rectangle (-5) 5 25 (-25))) 200 (withKeys (Char 'a') (Char 'z')) orange
p2paddle = Paddle (150, 0)  (Rect (Rectangle (-5) 5 25 (-25))) 200 (withKeys (Char '\'') (Char '/')) blue

paddleList = [p1paddle, p2paddle]

playerList :: [Player]
playerList =
  [ Player
    { paddle = p1paddle
    , score = Score (-120, 125) 0
    , hue = orange
    , endzone = Block (Rect (Rectangle 190 200 100 (-100))) white
    , index = 1
    }
  , Player
    { paddle = p2paddle
    , score = Score (80, 125) 0
    , hue = blue
    , endzone = Block (Rect (Rectangle (-200) (-190) 100 (-100))) white
    , index = 2
    }
  ]

main :: IO ()
main = do sprite <- loadBMP "resources/sprites/ball.bmp"
          let initBall = Ball (20, 0) (200, 300) sprite
          let initialWorld = World
                              { scenery = scene
                              , ball = initBall
                              , initialBall = initBall
                              , events = []
                              , players = playerList
                              }
          play window background fps initialWorld render listen update
