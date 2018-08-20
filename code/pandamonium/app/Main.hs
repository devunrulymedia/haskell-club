module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Renderable
import Redux
import World.Assets
import World.CreateWorld
import World.Stages.Stage1
import Game.Game

window :: Display
window = FullScreen

background :: Color
background = makeColor 0.1 0.2 0.5 1

fps :: Int
fps = 600

main :: IO ()
main = do assets <- loadAssets
          let world = createWorld assets stage1
          let game = withWorld world
          playIO window background fps game iorender (reduxListen gameRedux) (reduxUpdate gameRedux)
