module Pandamonium.Entities.Panda.Collisions where

import Graphics.Gloss.Data.Vector
import Pandamonium.Entities.Panda.MovementStateMachine
import Pandamonium.Entities.EntityTypes
import Pandamonium.Game.GameEvent
import Pandamonium.Systems.Controller

resetCollisions :: MovementState -> MovementState
resetCollisions _ = Airborne

processCollisions :: Vector -> MovementState -> MovementState
processCollisions (x, y) ms
  | y > 0 = Grounded
  | y == 0 && x < 0 && ms /= Grounded = WallHugging DRight
  | y == 0 && x > 0 && ms /= Grounded = WallHugging DLeft
  | otherwise = ms

handleCollisions :: GameEvent -> MovementState -> MovementState
handleCollisions ResetCollisions = resetCollisions
handleCollisions (Collision EPanda _ _ _ nv) = processCollisions nv
handleCollisions _ = id
