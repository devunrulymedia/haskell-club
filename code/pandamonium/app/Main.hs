module Main(
  main
) where

import System.Environment

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Common.Renderable
import Common.Redux
import Pandamonium.World.Assets
import Pandamonium.World.CreateWorld
import Pandamonium.Game.Game
import Pandamonium.World.Stages.Stage1
import Pandamonium.World.Stages.Stage2
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
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["thrust"] = thrust
parseArgs ["panda"] = pandamonium
parseArgs _ = putStrLn "Call with thrust for thrust, or panda for pandamonium"

playGame :: (IORenderable a) => a -> Redux a e -> IO ()
playGame game redux = playIO window background fps game iorender (reduxListen redux) (reduxUpdate redux)

thrust :: IO ()
thrust = do assets <- Thrust.World.Assets.loadAssets
            let world = Thrust.World.CreateWorld.createWorld assets
            let game = Thrust.Game.Game.withWorld world
            playGame game Thrust.Game.Game.gameRedux

pandamonium :: IO ()
pandamonium = do assets <- Pandamonium.World.Assets.loadAssets
                 let game = Pandamonium.Game.Game.withStages assets (cycle [stage2, stage1])
                 playGame game Pandamonium.Game.Game.gameRedux
