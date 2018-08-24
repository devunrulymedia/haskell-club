{-# LANGUAGE TemplateHaskell #-}

module Pandamonium.Entities.Panda.Panda where

import Graphics.Gloss
import Pandamonium.Shapes.Shape
import Pandamonium.Entities.Panda.MovementStateMachine
import Control.Lens
import Pandamonium.Systems.Controller

data Impulse = Impulse Float Vector

data Panda = Panda
  { _sprites :: [ Picture ]
  , _pos :: Vector
  , _vel :: Vector
  , _state :: MovementState
  , _impulse :: Maybe Impulse
  , _facing :: Direction
  , _controller :: Controller
  }

mkPanda :: [ Picture ] -> Vector -> Controller -> Panda
mkPanda s p c = Panda s p (0, 0) Airborne Nothing DRight c

makeLenses ''Panda

instance Movable Panda where
  move dv pd = pos %~ (+dv) $ pd

instance Moving Panda where
  velocity pd = pd ^. vel
  applyImpulse da pd = vel %~ (+da) $ pd
