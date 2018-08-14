module Entities.Panda.Animation where

import Graphics.Gloss
import Renderable
import Entities.Panda.Panda
import Control.Lens

instance Renderable Panda where
  render pd = let (x, y) = pd ^. pos in translate x y $ scale 2 2 $ (pd ^. sprites) !! 0
