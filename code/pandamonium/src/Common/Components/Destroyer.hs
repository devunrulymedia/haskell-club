{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module Common.Components.Destroyer where

import Control.Lens (Lens)

import Common.Redux
import Common.Relationship
import Common.Components.Entity

data Destroy = Destroy EntityId deriving ReduxEvent

destroy :: Destroy -> [ Entity ] -> IOEvents [ Entity ]
destroy (Destroy entityId) entities = return $ filter (\e -> from e /= Just entityId) entities

data Spawn = Spawn Entity deriving ReduxEvent

spawn :: Spawn -> [ Entity ] -> EntityId -> IOEvents ([Entity], EntityId)
spawn (Spawn entity) entities entityId =
  let next = succ entityId
   in return ((entity <-+ next) : entities, next)

lifecycle :: Lens w w [Entity] [Entity]
          -> Lens w w EntityId EntityId
          -> Redux w
lifecycle entities entityId = noOpRedux
  { reducer = composeHandler
      [ lensing entities (focusM destroy)
      , focusM (relationshipM spawn entities entityId)
      ]
  }
