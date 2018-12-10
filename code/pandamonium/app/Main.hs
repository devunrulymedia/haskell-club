module Main(
  main
) where

import System.Environment

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Common.Renderable
import Common.Redux
import Common.Exit
import Pandamonium.World.Assets
import Pandamonium.World.CreateWorld
import Pandamonium.Game.Game
import Pandamonium.World.Stages.Stage1
import Pandamonium.World.Stages.Stage2
import Panda2.World
import Panda2.Assets
import Thrust.World.Assets
import Thrust.World.CreateWorld
import Thrust.Game.Game
import Balls.World.World
import Fireworks.Game
import Bomberman.Game

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
parseArgs ["balls"] = ballpit
parseArgs ["fireworks"] = fireworks
parseArgs ["bomberman"] = bomberman
parseArgs ["panda2"] = panda2
parseArgs _ = putStrLn "Call with thrust for thrust, or panda for pandamonium"

playGame :: (Renderable a) => a -> Redux a -> IO ()
playGame game redux = playIO window background fps game iorender (exitable redux) (reduxUpdate redux)


thrust :: IO ()
thrust = do assets <- Thrust.World.Assets.loadAssets
            let world = Thrust.World.CreateWorld.createWorld assets
            let game = Thrust.Game.Game.withWorld world
            playGame game Thrust.Game.Game.gameRedux

pandamonium :: IO ()
pandamonium = do assets <- Pandamonium.World.Assets.loadAssets
                 let game = Pandamonium.Game.Game.withStages assets (cycle [stage2, stage1])
                 playGame game Pandamonium.Game.Game.gameRedux

ballpit :: IO ()
ballpit = do let world = Balls.World.World.world
             world' <- reduxDo ballsRedux world initialiseWorld
             playGame world' ballsRedux

fireworks :: IO ()
fireworks = do game <- buildGame
               playGame game Fireworks.Game.gameRedux

bomberman :: IO ()
bomberman = do game <- bombermanGame
               playGame game bgameRedux

panda2 :: IO ()
panda2 = do assets <- Panda2.Assets.loadAssets
            game <- newGame assets
            playGame game panda2Redux
