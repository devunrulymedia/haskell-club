{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Bomberman.Controller where

import Control.Lens
import Common.Components

data Controller = Controller
  { _vertical :: Axis
  , _horizontal :: Axis
  , _dropBomb :: Button
  } deriving Component

makeLenses ''Controller

defaultController :: Controller
defaultController = Controller
  { _vertical = axis (button '/') (button '@')
  , _horizontal = axis (button 'z') (button 'x')
  , _dropBomb = button ' '
  }

speed :: OnAxis -> Float
speed Min = -200
speed Neutral = 0
speed Max = 200

move :: a -> Controller -> Velocity
move _ controller = let xSpeed = speed (controller ^. horizontal . onAxis)
                        ySpeed = speed (controller ^. vertical . onAxis)
                     in Velocity (xSpeed, ySpeed)
