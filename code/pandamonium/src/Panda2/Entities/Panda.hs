module Panda2.Entities.Panda where

import Control.Lens
import Graphics.Gloss (Picture)
import Common.Components

panda :: [ Picture ] -> Entity
panda pandas = entity
           <-+ Position (20, 20)
           <-+ Sprite (head pandas)
