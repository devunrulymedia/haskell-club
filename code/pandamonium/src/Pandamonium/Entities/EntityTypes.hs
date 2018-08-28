module Pandamonium.Entities.EntityTypes where

import Common.Entities.Entity

data EntityType = EBlock
                | ECoin
                | EPanda

type Ent = Entity EntityType Integer
