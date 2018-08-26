{-# LANGUAGE TemplateHaskell #-}

module Common.Entities.Entity where

import Control.Lens
import Common.Renderable

data Entity t i d = Entity
  { _etype :: t
  , _eid :: i
  , _edata :: d
  }

makeLenses ''Entity

instance (Renderable d) => Renderable (Entity t i d) where
  render d = render (d ^. edata)