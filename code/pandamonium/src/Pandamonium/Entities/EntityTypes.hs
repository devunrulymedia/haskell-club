module Pandamonium.Entities.EntityTypes where

import Common.Entities.Entity

data EntityType = EBlock
                | ECoin
                | EPanda
                deriving Show

type Ent = Entity EntityType Integer
