{-# LANGUAGE TemplateHaskell #-}

module Pandamonium.Systems.Controller where

import Graphics.Gloss.Interface.IO.Game
import Common.Redux
import Pandamonium.Game.GameEvent

type ControlUpdater = Event -> ControlState -> Events GameEvent ControlState

type LeftPressed = Bool
type RightPressed = Bool
type JumpPressed = Bool

data ControlState = ControlState LeftPressed RightPressed JumpPressed

data Controller = Controller ControlState ControlUpdater

updateControlState :: Key -> Key -> Key -> ControlUpdater
updateControlState leftKey rightKey jumpKey = update where
  pressed Up = False
  pressed Down = True
  update (EventKey key state _ _) (ControlState left right jump)
    | key == leftKey   = return $ ControlState (pressed state) right jump
    | key == rightKey  = return $ ControlState left (pressed state) jump
    | key == jumpKey   = if pressed state
       then do fireEvent JumpPressed
               return $ ControlState left right (pressed state)
       else return $ ControlState left right (pressed state)
    | otherwise        = return $ ControlState left right jump
  update _ state = return $ state

leftPressed :: Controller -> Bool
leftPressed (Controller (ControlState l _ _) _ ) = l

rightPressed :: Controller -> Bool
rightPressed (Controller (ControlState _ r _) _ ) = r

jumpPressed :: Controller -> Bool
jumpPressed (Controller (ControlState _ _ j) _ ) = j

withKeys :: Key -> Key -> Key -> Controller
withKeys leftKey rightKey jumpKey = Controller (ControlState False False False) (updateControlState leftKey rightKey jumpKey)

data DPad = JLeft | JRight | JNeutral deriving (Eq, Show)
data Button = Pressed | Released deriving (Eq, Show)

data Joypad = Joypad DPad Button

toJoypad :: Controller -> Joypad
toJoypad (Controller (ControlState l r j) _) = Joypad
  (direction l r)
  (if j then Pressed else Released)
  where
    direction True False = JLeft
    direction False True = JRight
    direction _ _ = JNeutral

updateController :: Event -> Controller -> Events GameEvent Controller
updateController event (Controller state update) = do
  state' <- (update event state)
  return $ Controller state' update
