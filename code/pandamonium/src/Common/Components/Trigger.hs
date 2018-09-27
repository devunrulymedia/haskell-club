{-# LANGUAGE DeriveAnyClass #-}

module Common.Components.Trigger where

import Data.ConstrainedDynamic

import Common.Redux
import Common.Timer
import Common.Components.Entity
import Common.Components.Lifecycle

data Trigger = Trigger DynEvent deriving Component

updateTrigger :: Float -> Entity -> Events Entity
updateTrigger time entity = case (from entity) of
  Nothing -> return entity
  (Just (Trigger event)) -> do fireDynEvent event
                               destroy entity
                               return entity

delayedSpawn :: Float -> Entity -> Entity
delayedSpawn delay ent = entity
                     <-+ Trigger (toDyn (Await delay (toDyn (Spawn ent))))

triggerRedux :: Redux Entity
triggerRedux = noOpRedux { updater = updateTrigger }
