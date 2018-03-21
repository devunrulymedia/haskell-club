module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Codec.BMP
import Renderable
import Updatable
import Assets
import CreateWorld

window :: Display
window = InWindow "A window!" (500, 500) (100, 100)

background :: Color
background = black

fps :: Int
fps = 600

main :: IO ()
main = do assets <- loadAssets
          let world = createWorld assets
          play window background fps world render listen update
