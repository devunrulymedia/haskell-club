{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Pandamonium.World.World where

import Control.Lens
import Control.Arrow
import Control.Monad.Writer
import Data.DList

import Data.Maybe
import Data.Dynamic
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game

import Common.Shapes.Shape
import Common.Renderable
import Common.Redux
import Common.Relationship
import Common.Entities.Entity
import Common.Entities.TypeClasses.Shapes
import Common.Entities.Block
import Common.Entities.Destroyer
import Common.Entities.Spawner
import Common.Physics.Collisions

import Pandamonium.Entities.EntityTypes
import Pandamonium.Entities.Panda
import Pandamonium.Entities.Coin
import Pandamonium.Game.GameEvent

data World = World
  { _scenery :: [ Ent Block ]
  , _panda :: [ Ent Panda ]
  , _coins :: [ Ent Coin ]
  , _entityindex :: Integer
  }

makeLenses ''World

type Listener = Event -> World -> Events World
type Updater  = Float -> World -> Events World
type Reducer  = GameEvent -> World -> IOEvents World

instance Renderable World where
  render world = Pictures $
                   (render <$> world ^. scenery) ++
                   (render <$> world ^. coins) ++
                   (render <$> world ^. panda)

peek :: Monad m => (a -> m b) -> a -> m a
peek f a = do f a; return a

poke :: Monad m => m b -> a -> m a
poke m a = do m; return a

handleCollisions :: Float -> World -> Events World
handleCollisions t = relationshipM (onPairs singleCollision) panda scenery where
  singleCollision p b = bounce_against_static 0 p b

pickupCoin :: Ent Panda -> Ent Coin -> Events ()
pickupCoin pd coin = touch pd coin

checkForPickups :: World -> Events World
checkForPickups w = do sequence (pure pickupCoin <*> w ^. panda <*> w ^. coins)
                       return w

checkForCompletion :: World -> Events World
checkForCompletion w = case w ^. coins of
  [] -> do fireEvent Cleared; return w
  otherwise -> return w

spawnEntity :: Typeable a => EntityType -> Lens World World ([Ent a]) ([Ent a]) -> Spawn -> World -> World
spawnEntity etype field = relationshipWith (spawn etype) field entityindex

reduceWorld :: Spawn -> World -> IOEvents World
reduceWorld e w = return w
              <&> spawnEntity ECoin coins e
              <&> spawnEntity EBlock scenery e
              <&> spawnEntity EPanda panda e

updateWorld :: Updater
updateWorld t w = return w
              >>= poke (fireEvent ResetCollisions)
              >>= checkForPickups
              >>= handleCollisions t
              >>= checkForCompletion

topLevelRedux :: Redux World
topLevelRedux = Redux
  { reducer  = composeHandler [ focusM reduceWorld ]
  , listener = noOp
  , updater  = updateWorld
  }

worldRedux :: Redux World
worldRedux = compose
  [
  connect pandaRedux (panda . traverse),
  connect coinRedux (coins . traverse),
  connect destroyer coins,
  topLevelRedux
  ]
