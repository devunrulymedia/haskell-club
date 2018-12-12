{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Panda2.Controller where

import Control.Lens
import Control.Monad.Trans
import Graphics.Gloss.Interface.IO.Game

import Common.Redux
import Common.Controls.Button
import Common.Controls.Axis

data Controller = Controller
  { _playerNumber :: Integer
  , _horizontal :: Axis
  , _jump :: Button
  }

makeLenses ''Controller

data JumpPressed = JumpPressed Integer deriving ReduxEvent
data JumpReleased = JumpReleased Integer deriving ReduxEvent

listenController :: Event -> Controller -> Events Controller
listenController event controller = return controller
                                >>= jump %%~ keyPress event
                                >>= horizontal %%~ axisPress event

newController :: Integer -> (Char, Char) -> Char -> Controller
newController p (l, r) j = let left = button l
                               right = button r
                               jump = onPress .~ fires (JumpPressed p)
                                    $ onRelease .~ fires (JumpReleased p)
                                    $ button j
                            in Controller
                                 { _playerNumber = p
                                 , _horizontal = axis left right
                                 , _jump = jump
                                 }

printJumps :: JumpPressed -> a -> IOEvents a
printJumps (JumpPressed x) a = do
  liftIO $ print ("Jump pressed on controller" ++ show x)
  return a

controllerRedux :: Redux Controller
controllerRedux = Redux
  { listener = listenController
  , updater  = noOp
  , reducer  = focusM printJumps
  }
