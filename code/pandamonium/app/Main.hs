module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Renderable
import Updatable
import World.Assets
import World.CreateWorld

window :: Display
window = InWindow "A window!" (500, 500) (100, 100)

background :: Color
background = black

fps :: Int
fps = 600

main :: IO ()
main = do assets <- loadAssets
          let world = createWorld assets
          playIO window background fps world iorender iolisten ioupdate
