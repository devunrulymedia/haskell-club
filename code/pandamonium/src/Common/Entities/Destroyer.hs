{-# LANGUAGE MultiParamTypeClasses #-}

module Common.Entities.Destroyer where

import Control.Lens

import Common.Redux
import Common.Entities.Entity

class Destroys i e where
  destroys :: e -> Maybe i

destroyEntities :: (Eq i, Destroys i e) => e -> [ Entity t i a] -> IOEvents e [ Entity t i a ]
destroyEntities e xs = case (destroys e) of
  (Nothing) -> return xs
  (Just i)  -> return $ filter survivesPurge xs where
    survivesPurge entity = entity ^. eid /= i

destroyer :: (Eq i, Destroys i e) => Redux [ Entity t i a ] e
destroyer = Redux
  { reducer = destroyEntities
  , updater = noOp
  , listener = noOp
  }
