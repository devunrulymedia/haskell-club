module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Thrust.Renderable
import Thrust.Redux
import Thrust.World.Assets
import Thrust.World.CreateWorld
import Thrust.Game.Game

window :: Display
window = FullScreen

background :: Color
background = black

fps :: Int
fps = 600

main :: IO ()
main = thrust

thrust :: IO ()
thrust = do assets <- loadAssets
            let world = createWorld assets
            let game = withWorld world
            playIO window background fps game iorender (reduxListen gameRedux) (reduxUpdate gameRedux)

-- pandamonium :: IO ()
-- pandamonium = do assets <- loadAssets
--           let game = withStages assets (cycle [stage2, stage1])
--           playIO window background fps game iorender (reduxListen gameRedux) (reduxUpdate gameRedux)
