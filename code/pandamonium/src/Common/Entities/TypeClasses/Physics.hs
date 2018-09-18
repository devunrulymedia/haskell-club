module Common.Entities.TypeClasses.Physics where

import Control.Lens

import Common.Entities.Entity
import Common.Entities.TypeClasses.Shapes
import Common.Physics.Physics

instance (Physics d) => Physics (Entity t i d) where
  mass d = mass (d ^. edata)
  elasticity d = elasticity (d ^. edata)
