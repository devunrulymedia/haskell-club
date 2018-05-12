{-# LANGUAGE TemplateHaskell #-}


module Entities.Jumpman where

import Control.Lens
import Shapes.Shape
import Renderable
import Updatable
import Systems.Controller
import Graphics.Gloss (color, yellow, Vector, Picture)

hv :: Float
hv = 200

data GroundedState = Grounded | Aerial

data Jumpman = Jumpman
  { _pos :: Vector
  , _vel :: Vector
  , _grounded :: GroundedState
  , _controller :: Controller
  }

makeLenses ''Jumpman

instance Shaped Jumpman where
  shape jm = Circle (jm ^. pos) 16

instance Renderable Jumpman where
  render jm = color yellow $ render $ shape jm

instance Movable Jumpman where
  move dv jm = pos %~ (+dv) $ jm

instance Moving Jumpman where
  velocity jm = jm ^. vel
  applyImpulse da jm = vel %~ (+da) $ jm

instance Updatable Jumpman where
  listen event jm = controller %~ updateController event $ jm
  update t jm = case jm ^. controller of
    (Controller (ControlState True False) _) -> pos %~ (+ (t*(-hv), 0)) $ jm
    (Controller (ControlState False True) _) -> pos %~ (+ (t*hv, 0)) $ jm
    otherwise -> jm
