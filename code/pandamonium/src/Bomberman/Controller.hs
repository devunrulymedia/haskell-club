{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Bomberman.Controller where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game

import Common.Components
import Common.Redux

data Controller = Controller
  { _owner :: EntityId
  , _vertical :: Axis
  , _horizontal :: Axis
  , _dropBomb :: Button
  } deriving Component

makeLenses ''Controller

defaultController :: EntityId -> Controller
defaultController entId = Controller
  { _owner = entId
  , _vertical = axis (button '/') (button '\'')
  , _horizontal = axis (button 'z') (button 'x')
  , _dropBomb = button ' '
  }

listenController :: Event -> Controller -> Events Controller
listenController event controller = return controller
                                >>= vertical %%~ axisPress event
                                >>= horizontal %%~ axisPress event
                                >>= dropBomb %%~ keyPress event
