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
import Shape
import Paddle
import Controller

window :: Display
window = InWindow "A window!" (500, 500) (100, 100)

background :: Color
background = black

fps :: Int
fps = 600

scene :: [Block]
scene = [ Block (Rect (Rectangle (-200) 200 100 90)) red
        , Block (Rect (Rectangle (-200) 200 (-90) (-100))) blue
        , Block (Rect (Rectangle (-200) (-190) 100 (-100))) green
        , Block (Rect (Rectangle 190 200 100 (-100))) orange
        , Block (Circ (Shape.Circle (-40, 0) 30)) white ]

paddleList :: [Paddle]
paddleList = [ Paddle (-150, 0) (Rect (Rectangle (-10) 10 40 (-40))) 200 (withKeys (Char 'a') (Char 'z')) yellow ]

initialWorld :: World
initialWorld = World { scenery = scene, paddles = paddleList, ball = Ball (20, 0) (200, 150) }

main :: IO ()
main = do sprite <- loadBMP "resources/sprites/ugliness.bmp"
          play window background fps initialWorld render listen update
