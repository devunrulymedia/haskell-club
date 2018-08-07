{-# LANGUAGE TemplateHaskell #-}

module Entities.Panda.Panda where

import Graphics.Gloss
import Entities.Panda.MovementStateMachine
import Control.Lens
import Systems.Controller

data Panda = Panda
  { _sprite :: Picture
  , _pos :: Vector
  , _vel :: Vector
  , _state :: MovementState
  , _controller :: Controller
  }

mkPanda :: Picture -> Vector -> Controller -> Panda
mkPanda s p c = Panda s p (0, 0) Falling c

makeLenses ''Panda
