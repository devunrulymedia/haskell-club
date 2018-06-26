module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Renderable
import Redux
import World.Assets
import World.CreateWorld
import Game.Game

window :: Display
window = FullScreen

background :: Color
background = black

fps :: Int
fps = 600

main :: IO ()
main = do assets <- loadAssets
          let world = createWorld assets
          let game = withWorld world
          playIO window background fps game iorender (reduxListen gameRedux) (reduxUpdate gameRedux)
