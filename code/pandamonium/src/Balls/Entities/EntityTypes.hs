module Balls.Entities.EntityTypes where

import Common.Entities.Entity

data EntityType = EBall
                | EWall
                deriving Show

type Ent a = Entity Integer EntityType a
