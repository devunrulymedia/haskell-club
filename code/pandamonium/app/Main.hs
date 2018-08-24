module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Pandamonium.Renderable
import Pandamonium.Redux
import Pandamonium.World.Assets
import Pandamonium.World.CreateWorld
import Pandamonium.Game.Game
import Pandamonium.World.Stages.Stage1
import Pandamonium.World.Stages.Stage2
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

-- main :: IO ()
-- main = thrust

main :: IO ()
main = pandamonium


thrust :: IO ()
thrust = do assets <- Thrust.World.Assets.loadAssets
            let world = Thrust.World.CreateWorld.createWorld assets
            let game = Thrust.Game.Game.withWorld world
            playIO window background fps game Thrust.Renderable.iorender (Thrust.Redux.reduxListen Thrust.Game.Game.gameRedux) (Thrust.Redux.reduxUpdate Thrust.Game.Game.gameRedux)

pandamonium :: IO ()
pandamonium = do assets <- Pandamonium.World.Assets.loadAssets
                 let game = Pandamonium.Game.Game.withStages assets (cycle [stage2, stage1])
                 playIO window background fps game Pandamonium.Renderable.iorender (Pandamonium.Redux.reduxListen Pandamonium.Game.Game.gameRedux) (Pandamonium.Redux.reduxUpdate Pandamonium.Game.Game.gameRedux)
