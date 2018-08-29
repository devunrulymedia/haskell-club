{-# LANGUAGE TemplateHaskell #-}

module Pandamonium.World.World where

import Control.Lens
import Control.Arrow
import Control.Monad.Writer
import Data.DList

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game

import Common.Shapes.Shape
import Common.Renderable
import Common.Redux
import Common.Entities.Entity
import Common.Entities.TypeClasses.Shapes
import Common.Entities.Block
import Common.Entities.Destroyer
import Common.Physics.Collisions

import Pandamonium.Entities.EntityTypes
import Pandamonium.Entities.Panda
import Pandamonium.Entities.Coin
import Pandamonium.Game.GameEvent

data World = World
  { _scenery :: [ Ent Block ]
  , _panda :: Ent Panda
  , _coins :: [ Ent Coin ]
  , _score :: Int
  , _numbers :: [ Picture ]
  }

makeLenses ''World

type Listener = Event -> World -> Events GameEvent World
type Updater  = Float -> World -> Events GameEvent World
type Reducer  = GameEvent -> World -> IOEvents GameEvent World

instance Renderable World where
  render world = Pictures $
                   (render <$> world ^. scenery) ++
                   (render <$> world ^. coins) ++
                   (drawNumber 200 200 (world ^. score) (world ^. numbers)) ++
                   [render $ world ^. panda]

drawNumber :: Int -> Int -> Int -> [ Picture ] -> [ Picture ]
drawNumber x y 0 nums = []
drawNumber x y n nums = let (nextColumn, digit) = quotRem n 10
                            currentDigit = translate (fromIntegral x) (fromIntegral y) (nums !! digit)
                            remainingDigits = drawNumber (x - 16) y nextColumn nums
                         in currentDigit : remainingDigits

handleCollisions :: World -> Events GameEvent World
handleCollisions w = do
  tell $ singleton ResetCollisions
  panda %%~ (flip $ foldM $ bounce_against_static 0) (w ^. scenery) $ w

pickupCoin :: Ent Panda -> Ent Coin -> Events GameEvent ()
pickupCoin pd coin = touch pd coin

checkForPickups :: World -> Events GameEvent World
checkForPickups w = do traverse (pickupCoin $ w ^. panda) (w ^. coins)
                       return w

scoreCoin :: GameEvent -> World -> World
scoreCoin (Collision ECoin _ _ _ _) world = score +~ 5 $ world
scoreCoin _ world = world

respawnCoin :: GameEvent -> World -> World
respawnCoin (RespawnCoin coinId pos) world = coins %~ (Entity ECoin coinId (Coin pos) :) $ world
respawnCoin _ world = world

checkForCompletion :: World -> Events GameEvent World
checkForCompletion w = case w ^. coins of
  [] -> do fireEvent Cleared; return w
  otherwise -> return w

reduceWorld :: Reducer
reduceWorld e w = return w
              <&> scoreCoin e
              <&> respawnCoin e

updateWorld :: Updater
updateWorld t w = return w
              >>= checkForPickups
              >>= handleCollisions
              >>= checkForCompletion

topLevelRedux :: Redux World GameEvent
topLevelRedux = Redux
  { reducer  = reduceWorld
  , listener = noOp
  , updater  = updateWorld
  }

worldRedux :: Redux World GameEvent
worldRedux = compose
  [ connect pandaRedux (panda . edata)
  , connect (onAll coinRedux) coins
  , connect destroyer coins
  , topLevelRedux
  ]
