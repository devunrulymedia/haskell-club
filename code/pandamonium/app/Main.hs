module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Codec.BMP
import World
import Block
import Vector
import Ball
import Renderable
import Shape

window :: Display
window = InWindow "A window!" (200, 200) (300, 300)

background :: Color
background = black

fps :: Int
fps = 60

scene :: [Block]
scene = [ Block { shape = Rect (Rectangle (-40) (-10) 10 (-70)), col = red  }
        , Block { shape = Rect (Rectangle 20 50 40 (-100)),      col = blue } ]

initialWorld :: World
initialWorld = World { scenery = scene, ball = Ball { pos = Vector.Vector { x = 0, y = 0 }, velocity = Vector.Vector { x = 10, y = 10 } } }

onEvent :: Event -> World -> World
onEvent event world = world

onTime :: Float -> World -> World
onTime t world = let ballo = ball world
                     newPos = pos ballo + (velocity ballo * Vector { x = t, y = t })
                  in World { scenery = scenery world, ball = ballo { pos = newPos } }

main :: IO ()
main = do sprite <- loadBMP "resources/sprites/ugliness.bmp"
          play window background fps initialWorld render onEvent onTime
