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
import Common.Entities.Block

import Pandamonium.Entities.Panda
import Pandamonium.Entities.Coin
import Pandamonium.Systems.Physics
import Pandamonium.Game.GameEvent

data World = World
  { _scenery :: [ Block ]
  , _panda :: Panda
  , _coins :: [ Coin ]
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
  panda %%~ (flip $ foldM $ bounce 0) (w ^. scenery) $ w

pickupCoin :: Panda -> Coin -> Events GameEvent ()
pickupCoin pd coin@(Coin name loc) = if (shape pd !!! shape coin)
  then do fireEvent $ CoinPickedUp name
          fireEvent $ Trigger 5 (RespawnCoin name loc)
          return ()
  else return ()

checkForPickups :: World -> Events GameEvent World
checkForPickups w = do traverse (pickupCoin $ w ^. panda) (w ^. coins)
                       return w

removeCollectedCoins :: GameEvent -> World -> World
removeCollectedCoins (CoinPickedUp name) world = coins %~ (reject $ sameName name) $ world where
  sameName :: String -> Coin -> Bool
  sameName name (Coin name' _) = name == name'
  reject :: (a -> Bool) -> [a] -> [a]
  reject test = filter (\x -> not $ test x)
removeCollectedCoins _ world = world

respawnCoins :: GameEvent -> World -> World
respawnCoins (RespawnCoin name loc) world = coins %~ (Coin name loc :) $ world
respawnCoins _ world = world

scoreCoin :: GameEvent -> World -> World
scoreCoin (CoinPickedUp _) world = score +~ 5 $ world
scoreCoin _ world = world

checkForCompletion :: World -> Events GameEvent World
checkForCompletion w = case w ^. coins of
  [] -> do fireEvent Cleared; return w
  otherwise -> return w

reduceWorld :: Reducer
reduceWorld e w = return w
              <&> removeCollectedCoins e
              <&> scoreCoin e
              <&> respawnCoins e

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
  [ connect pandaRedux panda
  , topLevelRedux
  ]
