module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Codec.BMP
import World
import Block
import Ball
import CollisionHandler
import Integrator
import Gravity
import Renderable
import Shape

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

initialWorld :: World
initialWorld = World { scenery = scene, ball = Ball (20, 0) (200, 150) }

onEvent :: Event -> World -> World
onEvent event world = world

onTime :: Float -> World -> World
onTime t world = foldl (\w f -> f t w) world [gravitate 400, integrate, handleCollisions]

main :: IO ()
main = do sprite <- loadBMP "resources/sprites/ugliness.bmp"
          play window background fps initialWorld render onEvent onTime
