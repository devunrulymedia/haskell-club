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
import CollisionHandler
import Renderable
import Shape

window :: Display
window = InWindow "A window!" (500, 500) (100, 100)

background :: Color
background = black

fps :: Int
fps = 60

scene :: [Block]
scene = [ Block { shape = Rect (Rectangle (-200) 200 100 90), col = red  }
        , Block { shape = Rect (Rectangle (-200) 200 (-90) (-100)), col = blue }
        , Block { shape = Rect (Rectangle (-200) (-190) 100 (-100)), col = green }
        , Block { shape = Rect (Rectangle 190 200 100 (-100)), col = orange }
        , Block { shape = Circ (Shape.Circle Vector { x = -40, y = 0} 30), col = white }]

initialWorld :: World
initialWorld = World { scenery = scene, ball = Ball { pos = Vector.Vector { x = 0, y = 0 }, velocity = Vector.Vector { x = 200, y = 150 } } }

onEvent :: Event -> World -> World
onEvent event world = world

onTime :: Float -> World -> World
onTime t world = let ballo = ball world
                     newPos = pos ballo + (velocity ballo * Vector { x = t, y = t })
                  in handleWorld World { scenery = scenery world, ball = ballo { pos = newPos } }

main :: IO ()
main = do sprite <- loadBMP "resources/sprites/ugliness.bmp"
          play window background fps initialWorld render onEvent onTime
