{-# LANGUAGE TemplateHaskell #-}

module World.World where

import Control.Lens
import Control.Arrow
import System.Exit
import Control.Monad.Writer
import Data.DList

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game

import Entities.Block
import Entities.Thruster

import Game.GameEvent
import Shapes.Shape
import Renderable
import Redux

data World = World
  { _scenery :: [ Block ]
  , _thruster :: Thruster
  }

makeLenses ''World

type Listener = Event -> World -> Events GameEvent World
type Updater  = Float -> World -> Events GameEvent World
type Reducer  = GameEvent -> World -> IOEvents GameEvent World

instance IORenderable World where
  iorender world = pure $ Pictures $
                   (render <$> world ^. scenery) ++
                   [render $ world ^. jumpman]

drawNumber :: Int -> Int -> Int -> [ Picture ] -> [ Picture ]
drawNumber x y 0 nums = []
drawNumber x y n nums = let (nextColumn, digit) = quotRem n 10
                            currentDigit = translate (fromIntegral x) (fromIntegral y) (nums !! digit)
                            remainingDigits = drawNumber (x - 16) y nextColumn nums
                         in currentDigit : remainingDigits

integrate :: Float -> World -> World
integrate t w = thruster %~ accelerate t
                $ thruster %~ applyVelocity t
                $ w

bounce :: (Movable a, Moving a, Shaped a, Shaped b) => Float -> a -> b -> Events GameEvent a
bounce el a b = case (shape b !!> shape a) of
  Nothing -> return a
  (Just pushout) -> do
    fireEvent (JumpmanCollision offset)
    return (move offset (applyImpulse reflected_vel a)) where
      vel           = velocity a
      unit_push     = normalizeV pushout
      offset        = mulSV (1 + el) pushout
      normal_proj   = (1 + el) * (vel `dotV` unit_push)
      reflected_vel = negate $ mulSV normal_proj unit_push

handleCollisions :: World -> Events GameEvent World
handleCollisions w = thruster %%~ (flip $ foldM $ bounce 0) (w ^. scenery) $ w

scoreCoin :: GameEvent -> World -> World
scoreCoin (CoinPickedUp _) world = score +~ 5 $ world
scoreCoin _ world = world

exitOnEscape :: Event -> World -> IO World
exitOnEscape (EventKey key _ _ _) w = if key == Char 'q'
  then do exitSuccess
          return w
  else return w
exitOnEscape _ w = return w

listenForQuit :: Listener
listenForQuit (EventKey (Char 'q') _ _ _ ) w = do fireEvent Quit; return w
listenForQuit _ w = return w

listenWorld :: Listener
listenWorld e w = return w
              <&> thruster %~ collectEvents e
              >>= listenForQuit e

quit :: Reducer
quit Quit w = do liftIO exitSuccess; return w
quit _ w    = return w


changeBlockColour :: GameEvent -> World -> IOEvents GameEvent World
changeBlockColour (ChangeScenery) w = do
  tell $ singleton (TimedEvent 1 ChangeScenery)
  return $ scenery %~ (tint <$>) $ w where
    tint :: Block -> Block
    tint (Block shape color) = case rgbaOfColor color of
      (1,1,1,1)   -> Block shape (makeColor 0 1 1 1)
      (0,1,1,1)   -> Block shape (makeColor 1 0.5 1 1)
      (1,0.5,1,1) -> Block shape (makeColor 1 1 0.3 1)
      otherwise   -> Block shape (makeColor 1 1 1 1)
changeBlockColour _ w = return w

reduceWorld :: Reducer
reduceWorld e w = return w
              >>= changeBlockColour e
              >>= quit e

updateWorld :: Updater
updateWorld t w = return w
              <&> thruster %~ update t
              <&> integrate t

              >>= handleCollisions

topLevelRedux :: Redux World GameEvent
topLevelRedux = Redux
  { reducer  = reduceWorld
  , listener = listenWorld
  , updater  = updateWorld
  }

worldRedux :: Redux World GameEvent
worldRedux = compose
  [ connect jumpmanRedux jumpman
  , topLevelRedux
  ]
