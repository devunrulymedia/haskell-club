module Common.Entities.Spawner where

import Data.Dynamic
import Control.Monad.State

import Common.Entities.Entity

data Spawn = Spawn Dynamic

spawn :: (Enum i, Typeable d) => t -> [ Entity t i d ] -> Spawn -> State i [ Entity t i d ]
spawn typ entities (Spawn e) = case (fromDynamic e) of
  Nothing -> return entities
  (Just obj) -> do modify succ
                   nextIndex <- get
                   return $ Entity typ nextIndex obj : entities

spawn2 :: (Enum i, Typeable d) => t -> ([ Entity t i d ], i) -> Spawn -> ([ Entity t i d ], i)
spawn2 typ (entities, index) (Spawn e) = case (fromDynamic e) of
  Nothing    -> (entities, index)
  (Just obj) -> let nextIndex = succ index
                 in (Entity typ nextIndex obj : entities, nextIndex)
