module Panda2.Entities.Panda where

import Control.Lens
import Common.Components
import Panda2.Assets

panda :: Assets -> Entity
panda assets = entity
           <-+ Position (200, 200)
           <-+ Sprite (head $ assets ^. pandas)
