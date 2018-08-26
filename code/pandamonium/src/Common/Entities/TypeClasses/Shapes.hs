module Common.Entities.TypeClasses.Shapes where

import Control.Lens

import Common.Entities.Entity
import Common.Shapes.Shape

instance (Shaped d) => Shaped (Entity t i d) where
  shape d = shape (d ^. edata)

instance (Movable d) => Movable (Entity t i d) where
  move v = edata %~ move v

instance (Moving d) => Moving (Entity t i d) where
  velocity d = velocity (d ^. edata)
  applyImpulse v = edata %~ applyImpulse v
