{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module Common.Components.Lifecycle
  ( destroy
  , spawn
  , lifecycle
  , Lifespan (Lifespan)
  , Spawn (Spawn)
  , Destroy (Destroy)
  ) where

import Control.Lens (Lens, (<&>))
import Control.Monad

import Common.Redux
import Common.Relationship
import Common.Components.Entity

data Destroy = Destroy EntityId deriving ReduxEvent

data Lifespan = Lifespan Float deriving Component

age :: Float -> Lifespan -> Lifespan
age t (Lifespan a) = Lifespan (a - t)

dieOfOldAge :: Entity -> Events Entity
dieOfOldAge e = case extract e of
  Nothing -> return e
  (Just (Lifespan a)) -> do
    when (a < 0) (destroy e)
    return e

destroy :: Monad m => Entity -> EventsT m ()
destroy entity = destroy' (extract entity) where
  destroy' :: Monad m => Maybe EntityId -> EventsT m ()
  destroy' (Just entityId) = fireEvent (Destroy entityId)
  destroy' Nothing = return ()

doDestroy :: Destroy -> [ Entity ] -> IOEvents [ Entity ]
doDestroy (Destroy entityId) entities = return $ filter (\e -> extract e /= Just entityId) entities

data Spawn = Spawn Entity deriving ReduxEvent

spawn :: Monad m => Entity -> EventsT m ()
spawn entity = fireEvent (Spawn entity)

doSpawn :: Spawn -> [ Entity ] -> EntityId -> IOEvents ([Entity], EntityId)
doSpawn (Spawn entity) entities entityId =
  let next = succ entityId
   in return ((entity <-+ next) : entities, next)

updateLifecycle :: Float -> Entity -> Events Entity
updateLifecycle t e = return e
                  <&> update1 age t
                  >>= dieOfOldAge

lifecycle :: Lens w w [Entity] [Entity]
          -> Lens w w EntityId EntityId
          -> Redux w
lifecycle entities entityId = Redux
  { updater  = lensing entities (onEach updateLifecycle)
  , listener = noOp
  , reducer  = composeHandler
      [ lensing entities (focusM doDestroy)
      , focusM (relationshipM doSpawn entities entityId)
      ]
  }
