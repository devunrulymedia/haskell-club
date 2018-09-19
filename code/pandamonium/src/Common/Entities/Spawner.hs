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

spawnNow :: (Typeable a, Monad m) => a -> WriterT (DList DynEvent) m ()
spawnNow a = fireEvent (Spawn (toDyn a))

spawnIn :: (Typeable a, Monad m) => Float -> a -> WriterT (DList DynEvent) m ()
spawnIn t a = awaitEvent t (Spawn (toDyn a))

spawn :: (Enum i, Typeable d) => t -> ([ Entity t i d ], i) -> Spawn -> ([ Entity t i d ], i)
spawn typ (entities, index) (Spawn e) = case (fromDynamic e) of
  Nothing    -> (entities, index)
  (Just obj) -> let nextIndex = succ index
                 in (Entity typ nextIndex obj : entities, nextIndex)
