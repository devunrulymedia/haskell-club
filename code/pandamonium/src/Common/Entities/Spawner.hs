{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module Common.Entities.Spawner where

import Data.Dynamic
import Data.DList
import Control.Monad.Writer
import Control.Monad.State

import Common.Redux
import Common.Timer
import Common.Entities.Entity

data Spawn = Spawn Dynamic deriving (ReduxEvent)

spawnNow :: Typeable a => a -> Events ()
spawnNow a = fireEvent (Spawn (toDyn a))

spawnIn :: Typeable a => Float -> a -> Events ()
spawnIn t a = await t (spawnNow a)

spawn :: (Enum i, Typeable d) => t -> Spawn -> [ Entity t i d ] -> i -> ([ Entity t i d ], i)
spawn typ (Spawn e) entities index = case (fromDynamic e) of
  Nothing    -> (entities, index)
  (Just obj) -> let nextIndex = succ index
                 in (Entity typ nextIndex obj : entities, nextIndex)
