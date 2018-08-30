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
  }

makeLenses ''World

type Listener = Event -> World -> Events World
type Updater  = Float -> World -> Events World
type Reducer  = GameEvent -> World -> IOEvents World

instance Renderable World where
  render world = Pictures $
                   (render <$> world ^. scenery) ++
                   (render <$> world ^. coins) ++
                   [render $ world ^. panda]

handleCollisions :: World -> Events World
handleCollisions w = do
  fireEvent ResetCollisions
  panda %%~ (flip $ foldM $ bounce_against_static 0) (w ^. scenery) $ w

pickupCoin :: Ent Panda -> Ent Coin -> Events ()
pickupCoin pd coin = touch pd coin

checkForPickups :: World -> Events World
checkForPickups w = do traverse (pickupCoin $ w ^. panda) (w ^. coins)
                       return w

respawnCoin :: GameEvent -> World -> World
respawnCoin (RespawnCoin coinId pos) world = coins %~ (Entity ECoin coinId (Coin pos) :) $ world
respawnCoin _ world = world

checkForCompletion :: World -> Events World
checkForCompletion w = case w ^. coins of
  [] -> do fireEvent Cleared; return w
  otherwise -> return w

reduceWorld :: Reducer
reduceWorld e w = return w
              <&> respawnCoin e

updateWorld :: Updater
updateWorld t w = return w
              >>= checkForPickups
              >>= handleCollisions
              >>= checkForCompletion

topLevelRedux :: Redux World
topLevelRedux = Redux
  { reducer  = concrify reduceWorld
  , listener = noOp
  , updater  = updateWorld
  }

worldRedux :: Redux World
worldRedux = compose
  [ connect pandaRedux (panda . edata)
  , connect (onAll coinRedux) coins
  , connect destroyer coins
  , topLevelRedux
  ]
