module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Codec.BMP
import World
import Block
import Renderable
import Shape

window :: Display
window = InWindow "A window!" (200, 200) (300, 300)

background :: Color
background = black

fps :: Int
fps = 60

scene :: [Block]
scene = [ Block { shape = Rectangle { left = -40, right = -10, top = 10, bottom = -70}, col = red  }
               , Block { shape = Rectangle { left = 20, right = 50, top = 40, bottom = -100}, col = blue } ]

initialWorld :: World
initialWorld = World { scenery = scene, entities = [] }

onEvent :: Event -> World -> World
onEvent event world = world

onTime :: Float -> World -> World
onTime t world = world

main :: IO ()
main = do sprite <- loadBMP "resources/sprites/ugliness.bmp"
          play window background fps initialWorld render onEvent onTime
