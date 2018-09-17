module Pandamonium.Entities.Panda.Collisions where

import Graphics.Gloss.Data.Vector
import Common.Physics.Collisions
import Pandamonium.Entities.Panda.MovementStateMachine
import Pandamonium.Entities.EntityTypes
import Pandamonium.Game.GameEvent
import Pandamonium.Systems.Controller

processCollisions :: Collision EntityType Integer -> MovementState -> MovementState
processCollisions (Collision EPanda _ EBlock _ (x, y)) ms
  | y > 0 = Grounded
  | y == 0 && x < 0 && ms /= Grounded = WallHugging DRight
  | y == 0 && x > 0 && ms /= Grounded = WallHugging DLeft
  | otherwise = ms
processCollisions _ ms = ms

handleCollisions :: ResetCollisions -> MovementState -> MovementState
handleCollisions ResetCollisions _ = Airborne
