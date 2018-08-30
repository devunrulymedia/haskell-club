module Thrust.Systems.Controller where

import Graphics.Gloss.Interface.IO.Game
import Common.Redux
import Thrust.Game.GameEvent

type ControlUpdater = Event -> ControlState -> Events ControlState

type LeftPressed = Bool
type RightPressed = Bool
type ThrustPressed = Bool

data ControlState = ControlState LeftPressed RightPressed ThrustPressed

data Controller = Controller ControlState ControlUpdater

updateControlState :: Key -> Key -> Key -> ControlUpdater
updateControlState leftKey rightKey thrustKey = update where
  pressed Up = False
  pressed Down = True
  update (EventKey key state _ _) (ControlState left right thrust)
    | key == leftKey   = return $ ControlState (pressed state) right thrust
    | key == rightKey  = return $ ControlState left (pressed state) thrust
    | key == thrustKey = return $ ControlState left right (pressed state)
    | otherwise        = return $ ControlState left right thrust
  update _ state = return $ state

withKeys :: Key -> Key -> Key -> Controller
withKeys leftKey rightKey thrustKey = Controller (ControlState False False False) (updateControlState leftKey rightKey thrustKey)

updateController :: Event -> Controller -> Events Controller
updateController event (Controller state update) = do
  state' <- (update event state)
  return $ Controller state' update
