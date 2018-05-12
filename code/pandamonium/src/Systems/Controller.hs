module Systems.Controller where

import Graphics.Gloss.Interface.IO.Game

type ControlUpdater = Event -> ControlState -> ControlState

type LeftPressed = Bool
type RightPressed = Bool
type UnprocessedJump = Bool

data ControlState = ControlState LeftPressed RightPressed UnprocessedJump

data Controller = Controller ControlState ControlUpdater

updateControlState :: Key -> Key -> Key -> ControlUpdater
updateControlState leftKey rightKey jumpKey = update where
  pressed Up = False
  pressed Down = True
  update (EventKey key state _ _) (ControlState left right jump)
    | key == leftKey   = ControlState (pressed state) right jump
    | key == rightKey  = ControlState left (pressed state) jump
    | key == jumpKey   = ControlState left right (if pressed state then True else jump)
    | otherwise        = ControlState left right jump
  update _ state = state

withKeys :: Key -> Key -> Key -> Controller
withKeys leftKey rightKey jumpKey = Controller (ControlState False False False) (updateControlState leftKey rightKey jumpKey)

consumeJump :: Controller -> Controller
consumeJump (Controller (ControlState left right jump) update) = Controller (ControlState left right False) update

updateController :: Event -> Controller -> Controller
updateController event (Controller state update) = Controller (update event state) update
