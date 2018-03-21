module Systems.Controller where

import Graphics.Gloss.Interface.IO.Game

type ControlUpdater = Event -> ControlState -> ControlState

type UpPressed = Bool
type DownPressed = Bool

data ControlState = ControlState UpPressed DownPressed

data Controller = Controller ControlState ControlUpdater

updateControlState :: Key -> Key -> ControlUpdater
updateControlState upKey downKey = update where
  pressed Up = False
  pressed Down = True
  update (EventKey key state _ _) (ControlState up down)
    | key == upKey   = ControlState (pressed state) down
    | key == downKey = ControlState up (pressed state)
    | otherwise      = ControlState up down
  update _ state = state

withKeys :: Key -> Key -> Controller
withKeys upKey downKey = Controller (ControlState False False) (updateControlState upKey downKey)

updateController :: Event -> Controller -> Controller
updateController event (Controller state update) = Controller (update event state) update
