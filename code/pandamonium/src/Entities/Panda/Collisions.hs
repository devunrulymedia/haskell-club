module Entities.Panda.Collisions where

import Graphics.Gloss.Data.Vector
import Entities.Panda.MovementStateMachine
import Game.GameEvent
import Systems.Controller

resetCollisions :: MovementState -> MovementState
resetCollisions _ = Falling

processCollisions :: Vector -> MovementState -> MovementState
processCollisions (x, y) ms
  | y > 0 = Grounded
  | y == 0 && x < 0 && ms /= Grounded = WallHugging DRight
  | y == 0 && x > 0 && ms /= Grounded = WallHugging DLeft
  | otherwise = ms

handleCollisions :: GameEvent -> MovementState -> MovementState
handleCollisions ResetCollisions = resetCollisions
handleCollisions (PandaCollision nv) = processCollisions nv
handleCollisions _ = id
