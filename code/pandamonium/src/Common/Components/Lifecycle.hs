{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module Common.Components.Lifecycle
  ( destroy
  , spawn
  , spawnWithId
  , lifecycle
  , Spawned (Spawned)
  , Spawn (Spawn)
  , Destroy (Destroy)
  ) where

import Control.Lens (Lens, (<&>))
import Control.Monad

import Common.Redux
import Common.Relationship
import Common.Components.Entity
import Common.Components.World

data Destroy = Destroy EntityId deriving ReduxEvent

data Spawn = Spawn (EntityId -> Entity) deriving ReduxEvent

-- this event is fired after an event is spawned, as it's only now
-- you can get access to its entityId
data Spawned = Spawned Entity deriving ReduxEvent

destroy :: Monad m => Entity -> EventsT m ()
destroy entity = destroy' (extract entity) where
  destroy' :: Monad m => Maybe EntityId -> EventsT m ()
  destroy' (Just entityId) = fireEvent (Destroy entityId)
  destroy' Nothing = return ()

doDestroy :: Destroy -> [ Entity ] -> IOEvents [ Entity ]
doDestroy (Destroy entityId) entities = return $ filter (\e -> extract e /= Just entityId) entities

spawn :: Monad m => Entity -> EventsT m ()
spawn entity = spawnWithId (\entId -> entity)

spawnWithId :: Monad m => (EntityId -> Entity) -> EventsT m ()
spawnWithId entityCreator = fireEvent (Spawn entityCreator)

doSpawn :: Spawn -> [ Entity ] -> EntityId -> IOEvents ([Entity], EntityId)
doSpawn (Spawn entityCreator) entities entityId = do
      let next = succ entityId
      let spawnedEntity = (entityCreator next) <-+ next
      fireEvent $ Spawned spawnedEntity
      return (spawnedEntity : entities, next)

reduceLifecycle :: DynEvent -> World -> IOEvents World
reduceLifecycle e w = return w
                  >>= (lensing entities (focusM doDestroy)) e
                  >>= focusM (relationshipM doSpawn entities entityId) e

lifecycle :: Redux World
lifecycle = Redux
  { updater  = noOp
  , listener = noOp
  , reducer  = reduceLifecycle
  }
