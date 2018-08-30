{-# LANGUAGE MultiParamTypeClasses #-}

module Common.Entities.Destroyer where

import Control.Lens
import Data.Dynamic

import Common.Redux2
import Common.Entities.Entity

data Destroy i = Destroy i

destroyEntities :: Eq i => Destroy i -> [ Entity t i a] -> IOEvents [ Entity t i a ]
destroyEntities (Destroy i) xs = return $ filter survivesPurge xs where
    survivesPurge entity = entity ^. eid /= i

destroyer :: (Typeable i, Eq i) => Redux [ Entity t i a ]
destroyer = Redux
  { reducer = concrify destroyEntities
  , updater = noOp
  , listener = noOp
  }
