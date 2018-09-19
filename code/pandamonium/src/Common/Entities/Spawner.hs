{-# LANGUAGE DeriveAnyClass #-}

module Common.Entities.Spawner where

import Data.Dynamic
import Data.DList
import Control.Monad.Writer
import Control.Monad.State

import Common.Redux
import Common.Timer
import Common.Entities.Entity

data Spawn = Spawn Dynamic deriving (ReduxEvent)

spawnNow :: (Typeable a, Monad m) => a -> EventsT m ()
spawnNow a = fireEvent (Spawn (toDyn a))

spawnIn :: (Typeable a, Monad m) => Float -> a -> EventsT m ()
spawnIn t a = awaitEvent t (Spawn (toDyn a))

spawn :: (Enum i, Typeable d) => t -> Spawn -> [ Entity t i d ] -> i -> ([ Entity t i d ], i)
spawn typ (Spawn e) entities index = case (fromDynamic e) of
  Nothing    -> (entities, index)
  (Just obj) -> let nextIndex = succ index
                 in (Entity typ nextIndex obj : entities, nextIndex)
