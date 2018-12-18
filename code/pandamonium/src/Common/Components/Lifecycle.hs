{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module Common.Components.Lifecycle
  ( destroy
  , destroyIn
  , spawn
  , lifecycle
  , OnSpawn (OnSpawn)
  , OnDestroy (OnDestroy)
  , Spawn (Spawn)
  , Destroy (Destroy)
  ) where

import Control.Lens (Lens, (<&>))
import Control.Monad

import Common.Redux
import Common.Timer
import Common.Relationship
import Common.Components.Entity
import Common.Components.World

data Destroy = Destroy EntityId deriving ReduxEvent

data OnDestroy = OnDestroy (Entity -> IOEvents ()) deriving Component

data Spawn = Spawn (EntityId -> Entity) deriving ReduxEvent

data OnSpawn = OnSpawn (Entity -> IOEvents ()) deriving Component

destroy :: Entity -> Events ()
destroy entity = fireEvent (Destroy (entityId entity))

destroyIn :: Float -> Entity -> Events ()
destroyIn delay entity = awaitEvent delay $ Destroy (entityId entity)

doDestroy :: Destroy -> [ Entity ] -> IOEvents [ Entity ]
doDestroy _ [] = return []
doDestroy d@(Destroy x) (e : es) = if x == entityId e
  then do
    case extract e of
      (Just (OnDestroy event)) -> event e
      Nothing -> return ()
    doDestroy d es
  else do
    es' <- doDestroy d es
    return $ e : es'

spawn :: (EntityId -> Entity) -> Events ()
spawn entityCreator = fireEvent (Spawn entityCreator)

doSpawn :: Spawn -> [ Entity ] -> EntityId -> IOEvents ([Entity], EntityId)
doSpawn (Spawn entityCreator) entities entityId = do
      let next = succ entityId
      let spawnedEntity = entityCreator next
      case extract spawnedEntity of
        (Just (OnSpawn event)) -> event spawnedEntity
        Nothing -> return ()
      return (spawnedEntity : entities, next)

reduceLifecycle :: DynEvent -> World -> IOEvents World
reduceLifecycle e w = return w
                  >>= (lensing entities (focusM doDestroy)) e
                  >>= focusM (relationshipMWith doSpawn entities nextEntityId) e

lifecycle :: Redux World
lifecycle = Redux
  { updater  = noOp
  , listener = noOp
  , reducer  = reduceLifecycle
  }
