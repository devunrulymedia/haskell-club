{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module Common.Components.Lifecycle
  ( destroy
  , spawn
  , spawnWithId
  , lifecycle
  , OnSpawn (OnSpawn)
  , Spawn (Spawn)
  , Destroy (Destroy)
  ) where

import Control.Lens (Lens, (<&>))
import Control.Monad
import Data.ConstrainedDynamic

import Common.Redux
import Common.Relationship
import Common.Components.Entity
import Common.Components.World

data Destroy = Destroy EntityId deriving ReduxEvent

data OnDestroy = OnDestroy DynEvent deriving Component

data Spawn = Spawn (EntityId -> Entity) deriving ReduxEvent

data OnSpawn = OnSpawn DynEvent deriving Component

destroy :: Monad m => Entity -> EventsT m ()
destroy entity = destroy' (extract entity) where
  destroy' :: Monad m => Maybe EntityId -> EventsT m ()
  destroy' (Just entityId) = fireEvent (Destroy entityId)
  destroy' Nothing = return ()

doDestroy :: Destroy -> [ Entity ] -> IOEvents [ Entity ]
doDestroy _ [] = return []
doDestroy d@(Destroy entityId) (e : es) = if (Just entityId) == extract e
  then do
    case extract e of
      (Just (OnDestroy event)) -> fireDynEvent event
      Nothing -> return ()
    doDestroy d es
  else do
    es' <- doDestroy d es
    return $ e : es'

spawn :: Monad m => Entity -> EventsT m ()
spawn entity = spawnWithId $ const entity

spawnWithId :: Monad m => (EntityId -> Entity) -> EventsT m ()
spawnWithId entityCreator = fireEvent (Spawn entityCreator)

doSpawn :: Spawn -> [ Entity ] -> EntityId -> IOEvents ([Entity], EntityId)
doSpawn (Spawn entityCreator) entities entityId = do
      let next = succ entityId
      let spawnedEntity = (entityCreator next) <-+ next
      case extract spawnedEntity of
        (Just (OnSpawn event)) -> fireDynEvent event
        Nothing -> return ()
      return (spawnedEntity : entities, next)

reduceLifecycle :: DynEvent -> World -> IOEvents World
reduceLifecycle e w = return w
                  >>= (lensing entities (focusM doDestroy)) e
                  >>= focusM (relationshipMWith doSpawn entities entityId) e

lifecycle :: Redux World
lifecycle = Redux
  { updater  = noOp
  , listener = noOp
  , reducer  = reduceLifecycle
  }
