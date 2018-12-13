{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Panda2.Controller where

import Control.Lens
import Control.Monad.Trans
import Graphics.Gloss.Interface.IO.Game

import Common.Redux
import Common.Components
import Common.Controls.Button
import Common.Controls.Axis

data PlayerIndex = PlayerIndex Integer deriving (Eq, Component)

data Controller = Controller
  { _playerIndex :: PlayerIndex
  , _horizontal :: Axis
  , _jump :: Button
  }

makeLenses ''Controller

data JumpPressed = JumpPressed PlayerIndex deriving ReduxEvent
data JumpReleased = JumpReleased PlayerIndex deriving ReduxEvent

listenController :: Event -> Controller -> Events Controller
listenController event controller = return controller
                                >>= jump %%~ keyPress event
                                >>= horizontal %%~ axisPress event

newController :: Integer -> (Char, Char) -> Char -> Controller
newController p (l, r) j = let left = button l
                               right = button r
                               player = PlayerIndex p
                               jump = onPress .~ fires (JumpPressed player)
                                    $ onRelease .~ fires (JumpReleased player)
                                    $ button j
                            in Controller
                                 { _playerIndex = player
                                 , _horizontal = axis left right
                                 , _jump = jump
                                 }

printJumps :: JumpPressed -> a -> IOEvents a
printJumps (JumpPressed (PlayerIndex x)) a = do
  liftIO $ print ("Jump pressed on controller" ++ show x)
  return a

controllerRedux :: Redux Controller
controllerRedux = Redux
  { listener = listenController
  , updater  = noOp
  , reducer  = focusM printJumps
  }
