{-# LANGUAGE DeriveAnyClass #-}

module Common.Entities.Spawner where

import Data.Dynamic
import Control.Monad.State

import Common.Redux
import Common.Entities.Entity

data Spawn = Spawn Dynamic deriving (ReduxEvent)

makeSpawn :: (Typeable a) => a -> Spawn
makeSpawn a = Spawn (toDyn a)

spawn :: (Enum i, Typeable d) => t -> ([ Entity t i d ], i) -> Spawn -> ([ Entity t i d ], i)
spawn typ (entities, index) (Spawn e) = case (fromDynamic e) of
  Nothing    -> (entities, index)
  (Just obj) -> let nextIndex = succ index
                 in (Entity typ nextIndex obj : entities, nextIndex)
