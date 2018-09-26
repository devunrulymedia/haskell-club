{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module Common.Components.Lifecycle
  ( destroy
  , spawn
  , lifecycle
  , Spawn (Spawn)
  , Destroy (Destroy)
  ) where

import Control.Lens (Lens)

import Common.Redux
import Common.Relationship
import Common.Components.Entity

data Destroy = Destroy EntityId deriving ReduxEvent

destroy :: Monad m => Entity -> EventsT m ()
destroy entity = destroy' (from entity) where
  destroy' :: Monad m => Maybe EntityId -> EventsT m ()
  destroy' (Just entityId) = fireEvent (Destroy entityId)
  destroy' Nothing = return ()

doDestroy :: Destroy -> [ Entity ] -> IOEvents [ Entity ]
doDestroy (Destroy entityId) entities = return $ filter (\e -> from e /= Just entityId) entities

data Spawn = Spawn Entity deriving ReduxEvent

spawn :: Monad m => Entity -> EventsT m ()
spawn entity = fireEvent (Spawn entity)

doSpawn :: Spawn -> [ Entity ] -> EntityId -> IOEvents ([Entity], EntityId)
doSpawn (Spawn entity) entities entityId =
  let next = succ entityId
   in return ((entity <-+ next) : entities, next)

lifecycle :: Lens w w [Entity] [Entity]
          -> Lens w w EntityId EntityId
          -> Redux w
lifecycle entities entityId = noOpRedux
  { reducer = composeHandler
      [ lensing entities (focusM doDestroy)
      , focusM (relationshipM doSpawn entities entityId)
      ]
  }
