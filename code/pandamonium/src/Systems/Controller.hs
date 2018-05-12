module Systems.Controller where

import Graphics.Gloss.Interface.IO.Game

type ControlUpdater = Event -> ControlState -> ControlState

type LeftPressed = Bool
type RightPressed = Bool

data ControlState = ControlState LeftPressed RightPressed

data Controller = Controller ControlState ControlUpdater

updateControlState :: Key -> Key -> ControlUpdater
updateControlState leftKey rightKey = update where
  pressed Up = False
  pressed Down = True
  update (EventKey key state _ _) (ControlState left right)
    | key == leftKey   = ControlState (pressed state) right
    | key == rightKey  = ControlState left (pressed state)
    | otherwise        = ControlState left right
  update _ state = state

withKeys :: Key -> Key -> Controller
withKeys leftKey rightKey = Controller (ControlState False False) (updateControlState leftKey rightKey)

updateController :: Event -> Controller -> Controller
updateController event (Controller state update) = Controller (update event state) update
